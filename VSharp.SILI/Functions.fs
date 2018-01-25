﻿namespace VSharp.Interpreter

open VSharp
open VSharp.Core
open JetBrains.Decompiler.Ast
open JetBrains.Metadata.Reader.API

module Functions =

    let internal MakeLambdaTerm (signature : IFunctionSignature) (returnMetadataType : IMetadataType) (lambda : SymbolicLambda<'a>) =
        let typ = MetadataTypes.FromDecompiledSignature signature returnMetadataType
        MakeLambda lambda typ

    let internal MakeLambda state (metadataMethod : IMetadataMethod) (lambda : SymbolicLambda<'a>) =
        let typ = MetadataTypes.FromMetadataMethodSignature metadataMethod
        let term = Concrete lambda typ
        Memory.AllocateInHeap state term

    let internal MakeLambda2 state (signature : IFunctionSignature) (returnMetadataType : IMetadataType) (lambda : SymbolicLambda<'a>) =
        let term = MakeLambdaTerm signature returnMetadataType lambda
        if Transformations.isInlinedSignatureCall signature
            then Memory.AllocateInHeap state term
            else term, state
