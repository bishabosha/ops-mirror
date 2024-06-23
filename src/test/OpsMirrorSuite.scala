package example

import mirrorops.OpsMirror
import mirrorops.Meta
import compiletime.constValue
import compiletime.constValueTuple

class OpsMirrorSuite extends munit.FunSuite:
  import OpsMirrorSuite.*
  import OpMeta.*
  import ParamMeta.*

  class failsWith[E] extends mirrorops.ErrorAnnotation[E]

  enum BasicError:
    case Message(msg: String)

  enum OpMeta extends mirrorops.MetaAnnotation:
    case Streaming()
    case JSONBody()

  enum ParamMeta extends mirrorops.MetaAnnotation:
    case PrimaryKey()

  @failsWith[BasicError]
  trait BasicService:
    @Streaming
    @JSONBody
    def lookup(@PrimaryKey id: Long): String
  end BasicService

  test("summon mirror basic with annotations") {
    val mirror = summon[OpsMirror.Of[BasicService]]

    type FirstOp = Tuple.Head[mirror.MirroredOperations]

    summon[mirror.MirroredLabel =:= "BasicService"]
    summon[mirror.MirroredOperationLabels =:= ("lookup" *: EmptyTuple)]
    summon[Operation_Metadata[FirstOp] =:= (Meta @JSONBody, Meta @Streaming)]
    summon[Operation_InputLabels[FirstOp] =:= ("id" *: EmptyTuple)]
    summon[Operation_InputTypes[FirstOp] =:= (Long *: EmptyTuple)]
    summon[
      Operation_InputMetadatas[FirstOp] =:= ((Meta @PrimaryKey *: EmptyTuple) *: EmptyTuple)
    ]
    summon[Operation_ErrorType[FirstOp] =:= BasicError]
    summon[Operation_OutputType[FirstOp] =:= String]
  }
end OpsMirrorSuite

object OpsMirrorSuite:
  type Operation_Is[Ls <: Tuple] = mirrorops.Operation { type InputTypes = Ls }
  type Operation_Im[Ls <: Tuple] = mirrorops.Operation {
    type InputMetadatas = Ls
  }
  type Operation_M[Ls <: Tuple]  = mirrorops.Operation { type Metadata = Ls }
  type Operation_Et[E]           = mirrorops.Operation { type ErrorType = E }
  type Operation_Ot[T]           = mirrorops.Operation { type OutputType = T }
  type Operation_IL[Ls <: Tuple] = mirrorops.Operation { type InputLabels = Ls }
  type Operation_InputLabels[Op] = Op match
    case Operation_IL[ls] => ls
  type Operation_InputTypes[Op] = Op match
    case Operation_Is[ls] => ls
  type Operation_ErrorType[Op] = Op match
    case Operation_Et[ls] => ls
  type Operation_OutputType[Op] = Op match
    case Operation_Ot[ls] => ls
  type Operation_InputMetadatas[Op] = Op match
    case Operation_Im[ls] => ls
  type Operation_Metadata[Op] = Op match
    case Operation_M[ls] => ls
end OpsMirrorSuite
