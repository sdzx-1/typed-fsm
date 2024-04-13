## 介绍McBride Indexed Monad

这里有Edward Kmett对Indexed Monad的[介绍](https://stackoverflow.com/questions/28690448/what-is-indexed-monad)。
如他所说，至少有三种indexed monad： 
  1. Bob Atkey
```haskell
class IMonad m where
  ireturn  ::  a -> m i i a
  ibind    ::  m i j a -> (a -> m j k b) -> m i k b
```
  2. Conor McBride 
```haskell
type a ~> b = forall i. a i -> b i 

class IMonad m where
  ireturn :: a ~> m a
  ibind :: (a ~> m b) -> (m a ~> m b)
```
  3. Dominic Orchard

没有详细叙述，仅仅链接到这个[演讲](https://github.com/dorchard/effect-monad/blob/master/docs/ixmonad-fita14.pdf)。


> 为了使用do语法，需要QualifiedDo扩展。QualifiedDo扩展允许我们仅重载(>>=),(>>)这两个运算符。在ghc 9.10.1之前，这个扩展存在[严重的问题](https://gitlab.haskell.org/ghc/ghc/-/issues/21206)。但这个[MR](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10140)修复了这些问题！！最新的ghc 9.10.1包含了这个MR。所以这个库需要你更新ghc版本到9.10.1。
>> 使用ghcup 安装 ghc 9.10.1 [这里有足够的帮助](https://discourse.haskell.org/t/ghc-9-10-1-alpha1-is-now-available/9048)

我要介绍的就是McBride Indexed Monad，最早的论文[在此](https://personal.cis.strath.ac.uk/conor.mcbride/Kleisli.pdf)。
以下是 IFunctor, IMonad, (>>=), (>>)的详细定义
```haskell
infixr 0 ~>

type f ~> g = forall x. f x -> g x

class IFunctor f where
  imap :: (a ~> b) -> f a ~> f b

class (IFunctor m) => IMonad m where
  ireturn :: a ~> m a
  ibind :: (a ~> m b) -> m a ~> m b

(>>=) :: (IMonad (m :: (x -> Type) -> x -> Type)) 
      => m a ix -> (a ~> m b) -> m b ix
m >>= f = ibind f m

data At :: Type -> k -> k -> Type where
  At :: a -> At a k k
  deriving (Typeable)

(>>) :: (IMonad (m :: (x -> Type) -> x -> Type)) 
     => m (At a j) i -> m b j -> m b i
m >> f = ibind (\(At _) -> f) m
```

以下是我对(\~>)的理解：通过GADT，让值包含类型的信息，然后通过(\~>), pattern match 将类型传递到后续的函数
```haskell
data V = A | B 

data SV :: V -> Type where  -- GADT， 让值包含类型信息
   SA :: SV A
   SB :: SV B

data SV1 :: V -> Type where
   SA1 :: SV1 A
   SB1 :: SV1 B

fun :: SV ~> SV1     --  type f ~> g = forall x. f x -> g x
fun sv = case sv of  --  x是任意的但f，g必须具有相同的x
     SA -> SA1       -- 通过模式匹配将具体类型状态传递到后续函数
     SB -> SB1


class (IFunctor m) => IMonad m where
  ireturn :: a ~> m a
  ibind :: (a ~> m b)  -- a包含的类型信息将传递给(m b), 这正是我们所需要的：外部输入对类型产生影响！
        -> m a ~> m b
``` 

## 介绍typed-fsm 
fsm表示[有限状态机](https://en.wikipedia.org/wiki/Finite-state_machine),它在程序中有广泛的应用。该库受[typed-protocols](https://github.com/input-output-hk/typed-protocols)的启发而成。

使用typed-fsm会经历以下五步：

1. 定义状态
2. 定义状态转移消息
3. 构建状态处理函数
4. 构建不同状态下事件到消息的函数
5. 运行状态处理函数

别担心，在下面的Mouse Motion例子中有详细的介绍。


typed-fsm 的核心很简单
```haskell
-- ps 表示状态

-- 状态转移消息的类型类
class StateTransMsg ps where
  data Msg ps (st :: ps) (st' :: ps)

-- 核心的AST，本质上我们所作的一切就是构建这个AST，然后再解释它
-- Operate m ia st 是IMonad的实例，同时它内部包含了一个 m
data Operate :: (Type -> Type) -> (ps -> Type) -> ps -> Type where
  IReturn :: ia (mode :: ps) -> Operate m ia mode
  LiftM
    :: ( SingI mode
       , Reify mode
       , SingI mode'
       , Reify mode'
       )
    => m (Operate m ia mode')
    -> Operate m ia mode
  In
    :: forall ps m (from :: ps) ia
     . (Msg ps from ~> Operate m ia)
    -> Operate m ia from

-- instance IFunctor
instance (Functor m) => IFunctor (Operate m) where
  imap f = \case
    IReturn ia -> IReturn (f ia)
    LiftM f' -> LiftM (fmap (imap f) f')
    In cont -> In (imap f . cont)

-- instance IMonad
instance (Functor m) => IMonad (Operate m) where
  ireturn = IReturn
  ibind f = \case
    IReturn ia -> (f ia)
    LiftM m -> LiftM (fmap (ibind f) m)
    In cont -> In (ibind f . cont)
```

typed-fsm仅包含两个核心函数：getInput, liftm。我们使用这两个函数就足以构建Operate。
整体行为表现为：不停从外部读取消息，并将它转换为内部monad的行为。

getInput 从外部读取消息
```haskell
getInput :: forall ps m (from :: ps). (Functor m) 
         => Operate m (Msg ps from) from
getInput = In ireturn
```

liftm 将普通m,提升到Operate
```haskell
liftm :: forall ps m (mode :: ps) a
       . (Functor m, SingI mode, Reify mode) 
      => m a -> Operate m (At a mode) mode
liftm m = LiftM (returnAt <$> m)
```
# Example
## run example
1. ghcup 安装 ghc 9.10.1   
2. cabal run example  --flags="BuildExample"

## mouse motion example
### 例子解释
![example](./data/png/hover.png)

> 图中有一个方框，当鼠标在方框外边时状态是Idle，
> 当鼠标移动到方框内部时，状态变成Over。

> 当进入Over状态时，会打开一个计时器，
>> 1. 如果计时器超时，那么进入Hover状态，取消计时器，并在鼠标右上方显示一些信息。
>> 2. 如果鼠标移出方框，那么状态变成Idle，并取消计时器。
>> 3. 如果鼠标在方框内移动，那么更新计时器，从而阻止进入Hover状态。

> 当进入Hover状态时
>> 1. 如果鼠标在方框内移动，那么打开一个新的计时器，并进入Over状态
>> 2. 如果鼠标移动到方框外，那么进入Idle状态


### 1. 定义状态
```haskell
data Motion
  = Idle
  | Over
  | Hover
  deriving (Show)
```
### 2. 定义状态转移消息 
![state trans msg](./data/png/motion_trans_msg.png)
```haskell
instance StateTransMsg Motion where
  data Msg Motion from to where
    MoveIn :: Point' -> Timestamp -> Msg Motion Idle Over
    -----------------
    MoveOut :: Msg Motion Over Idle
    InMove :: Point' -> Timestamp -> Msg Motion Over Over
    TimeoutH :: Msg Motion Over Hover
    -----------------
    HInMove :: Point' -> Timestamp -> Msg Motion Hover Over
    HMoveOut :: Msg Motion Hover Idle
```

### 3. 构建状态处理函数
```haskell
timeoutSize :: Int
timeoutSize = 400_000

myRegisterTimeout :: StateT MotionState IO (TimerManager, TimeoutKey)
myRegisterTimeout = do
  chan <- use channel
  liftIO $ do
    tm <- getSystemTimerManager
    tk <- registerTimeout tm timeoutSize (atomically $ writeTChan chan ())
    pure (tm, tk)
```

```haskell
idelHandler :: Op Motion MotionState Idle Idle  -- 初始状态 Idle，最终状态 Idle
idelHandler = I.do
  msg <- getInput
  case msg of
    MoveIn pos tms -> I.do                     -- 当鼠标移动到方框内部时产生的消息
      At tp <- liftm $ do   
        mousePos .= pos
        (tm, tk) <- myRegisterTimeout          -- 注册计时器
        pure (tm, tk, tms)
      overHandler tp                           -- 进入overHandler
```

```haskell
overHandler :: (TimerManager, TimeoutKey, Timestamp) -> Op Motion MotionState Idle Over  -- 初始状态Over， 最终状态 Idle
overHandler (tm, tk, oldtms) = I.do
  msg <- getInput
  case msg of
    MoveOut -> I.do                                -- 鼠标移出方框时产生的消息
      liftm $ liftIO $ unregisterTimeout tm tk     -- 取消计时器 
      idelHandler                                  -- 进入idleHandler
    InMove pos tms -> I.do     -- 鼠标在方框内部移动时产生的消息
      liftm $ do
        mousePos .= pos
      if tms - oldtms > 40    --比较InMove消息时间戳，让更新计时器的动作以较低的频率进行
        then I.do
          liftm $ liftIO $ updateTimeout tm tk timeoutSize   -- 更新计时器，阻止进入Hover状态
          overHandler (tm, tk, tms)               -- 进入overHandler，从状态转移图中可以看出InMove消息不会改变Over状态
        else overHandler (tm, tk, oldtms)
    TimeoutH -> I.do       -- 鼠标在方框中静置超过设定的400ms时产生的消息
      liftm $ do
        liftIO $ unregisterTimeout tm tk   -- 取消计时器
        pos <- use mousePos
        onHover .= Just (pos, [show oldtms, show pos]) 
      hoverHandler   -- 进入hoverHandler
```

```haskell
hoverHandler :: Op Motion MotionState Idle Hover  -- 初始状态 Hover，最终状态 Idle
hoverHandler = I.do
  msg <- getInput
  case msg of
    HInMove pos tms -> I.do  -- 鼠标在方框中移动时产生的消息
      At tp <- liftm $ do
        mousePos .= pos
        onHover .= Nothing
        (tm, tk) <- myRegisterTimeout  -- 注册计时器
        pure (tm, tk, tms)
      overHandler tp     -- 进入overHandler
    HMoveOut -> I.do  -- 鼠标移出方框时产生的消息
      liftm $ onHover .= Nothing
      idelHandler    -- 进入idelHandler
```

### 4. 构建不同状态下事件到消息的函数
事件代表外部输入事件，在这个例子中代表sdl的Event(这里用MyEvent包装).


消息代表状态转移消息，这个例子里是:
```haskell
    MoveIn :: Point' -> Timestamp -> Msg Motion Idle Over
    MoveOut :: Msg Motion Over Idle
    InMove :: Point' -> Timestamp -> Msg Motion Over Over
    TimeoutH :: Msg Motion Over Hover
    HInMove :: Point' -> Timestamp -> Msg Motion Hover Over
    HMoveOut :: Msg Motion Hover Idle
```
在不同的状态下，有不同的事件到消息的映射。
```haskell
data SomeMsg ps from
  = forall (to :: ps).
    (SingI to, Reify to) =>
    SomeMsg (Msg ps from to)

newtype GenMsg ps state event from
  = GenMsg (state -> event -> Maybe (SomeMsg ps from))

-- 表示在不同状态下，事件如何转换到消息
-- 通过dependent-map保证不会产生错误的消息
type State2GenMsg ps state event = DMap (Sing @ps) (GenMsg ps state event)
```


这个例子中的不同状态下的事件到消息的函数
```haskell
mouseDepMap :: State2GenMsg Motion MotionState MyEvent
mouseDepMap =
  D.fromList
    [ SIdel
        :=> GenMsg
          ( \(MotionState rect' _ _ _) event -> case event of
              MyMouseMotion (fmap fromIntegral -> p) tms ->
                if rect' `contains` p
                  then Just $ SomeMsg (MoveIn p tms)
                  else Nothing
              _ -> Nothing
          )
    , SOver
        :=> GenMsg
          ( \(MotionState rect' _ _ _) event -> case event of
              MyMouseMotion (fmap fromIntegral -> p) tms ->
                if rect' `contains` p
                  then Just $ SomeMsg (InMove p tms)
                  else Just $ SomeMsg MoveOut
              MyTimeout -> Just $ SomeMsg TimeoutH
          )
    , SHover
        :=> GenMsg
          ( \(MotionState rect' _ (Point mx my) _) event -> case event of
              MyMouseMotion (fmap fromIntegral -> p@(Point x y)) tms ->
                if rect' `contains` p
                  then
                    if abs (mx - x) < 30 && abs (my - y) < 30
                      then Nothing
                      else
                        Just $ SomeMsg (HInMove p tms)
                  else Just $ SomeMsg HMoveOut
              _ -> Nothing
          )
    ]
```
### 5. 运行状态处理函数

解释Operate m ia s 到 m。
这里的m使用了具体的(StateT state IO)。我倾向于在这里使用简单的State + IO 的monad。原则上我们不需要复杂的控制流monad，fsm本身就代表了控制流。runOp是一个很通用的函数，当然整个的解释代码非常简单，你可以随意定制自己的解释代码。

```haskell
runOp
  :: forall ps event state a (input :: ps) (output :: ps)
   . ( SingI input
     , Reify input
     , GCompare (Sing @ps)
     )
  => State2GenMsg ps state event    -- 不同状态下的事件到消息函数的对应表
  -> [event]                        -- 外部输入的事件列表
  -> Operate (StateT state IO) (At a output) input       -- Operate AST
  -> (StateT state IO) (OpResult ps (StateT state IO) a) -- 底层的m，这里是具体的(StateT state IO)
runOp dmp evns = \case
  IReturn (At a) -> pure (Right a)       
  LiftM m -> m Prelude.>>= runOp dmp evns
  In f -> do                  -- 解释 函数f
    case D.lookup (sing @input) dmp of -- 根据当前状态查找对应的 事件生成消息函数
      Nothing -> error "np"           -- 没找到，直接报错
      Just (GenMsg genMsg) -> loop evns  -- 找到了对应的函数
       where
        loop [] = pure $ Left $ SomeOperate (In f) -- 没有足够的事件，返回函数，等下一次有事件时再执行
        loop (et : evns') = do
          state' <- get
          case genMsg state' et of
            Nothing -> loop evns'   -- 函数没有产生消息， 丢弃事件，尝试下一个事件
            Just (SomeMsg msg) -> runOp dmp evns' (f msg) -- 函数产生了消息，f 使用消息生成新的Operate并解释
```

## type-fsm 的优势
  专注于正确的消息
  
  从顶到下的设计，方便重构

  利于构建复杂的状态机系统
   1. 类型保证在编写时不会产生错误的函数调用
   2. 类型系统的帮助，让我们能定义很多状态处理函数，然后放心的相互递归调用。

  有完整性检查，如果你模式匹配少写了一些项，编译器会发出警告，无效的项目也会有警告


## 一些自己的感受
  McBride Indexed Monad 不止于此。在构建阶AST段能够跟踪不同输入对类型的影响，特别适合处理具有交互的状态机模型。
  
  [type-protocols](https://github.com/input-output-hk/typed-protocols/issues/25) 类型化的通信协议,这个库专注于client-server之间的通信。我相信它应该很容易扩展到更多角色参与的通信协议。
  
  可以用于gui，game的开发。比如上面的mouse motion的示例

  有限状态机的应用非常广泛。我希望能在高级别类型系统编写代码，生成低级的代码，部署到对应的机器上。类似于[fir](https://gitlab.com/sheaf/fir/-/tree/master)。

## 有任何问题和建议都可以给我发消息。
任何希望利用这个库或者使用McBride Indexed Monad的项目我都有兴趣参加。
McBride Indexed Monad 对haskell有非同寻常的意义!!!
