# Revision history for typed-fsm

## 0.3.0.1-- 2024-8-04
* Fix getSomeOperateSing remvoe (SingKind ts) Constraint

## 0.3.0.0-- 2024-7-25
* Remove the SingI constraints from SomeMsg and AnyMsg.
* Modify the definition of LiftM and remove the SingI constraints.
* RunOperate and runOp both remove the SingI constraints.
* Reason for doing this: When explaining Ast, constraints can be easily converted into proofs, but proofs seem difficult to convert into constraints.

## 0.2.0.1-- 2024-7-22

* Fix getSomeOperateSing
* Improve runOperate
