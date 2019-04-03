module Project.Source exposing
    ( Emit(..)
    , Msg(..)
    , Source
    , allModuleRef
    , getModule
    , init
    , mapModule
    , setModule
    , update
    )

import Project.Label as Label
import Project.Source.Module as Module
import Project.Source.Module.PartDef as Def
import Project.Source.Module.PartDef.Expr as Expr
import Project.Source.Module.PartDef.Name as PartDefName
import Project.Source.Module.PartDef.Type as PartDefType
import Project.Source.Module.TypeDef as TypeDef
import Project.Source.ModuleIndex as ModuleIndex
import Project.Source.ModuleWithCache as ModuleWithCache
import Project.SourceIndex as SourceIndex
import Utility.Map


type Source
    = Source
        { core : ModuleWithCache.ModuleWithResult
        , coreInt32 : ModuleWithCache.ModuleWithResult
        , sampleModule : ModuleWithCache.ModuleWithResult
        }



{-
   type RootModule
       = RootModuleLibrary
           { name : Label.Label
           , author : Label.Label
           , originalName : Label.Label
           , version : String
           }
       | RootModuleModule Module
-}
--type Module
--    = Module
--        { module_ : ModuleWithCache.Module
--        , children : List Module
--        }


init : ( Source, List Emit )
init =
    let
        ( core, coreEmit ) =
            initCore

        ( coreInt32, coreInt32Emit ) =
            initCoreInt32

        ( sampleModule, sampleModuleEmit ) =
            initSampleModule
    in
    ( Source
        { core = core
        , coreInt32 = coreInt32
        , sampleModule = sampleModule
        }
    , coreEmit ++ coreInt32Emit ++ sampleModuleEmit
    )


type Msg
    = MsgModule
        { moduleIndex : SourceIndex.ModuleIndex
        , moduleMsg : ModuleWithCache.Msg
        }


type Emit
    = EmitModule
        { moduleIndex : SourceIndex.ModuleIndex
        , moduleEmit : ModuleWithCache.Emit
        }


update : Msg -> Source -> ( Source, List Emit )
update msg source =
    case msg of
        MsgModule { moduleIndex, moduleMsg } ->
            let
                ( newModule, emitList ) =
                    source
                        |> getModule moduleIndex
                        |> ModuleWithCache.update moduleMsg
            in
            ( source
                |> setModule moduleIndex newModule
            , emitList
                |> List.map
                    (\e ->
                        EmitModule
                            { moduleIndex = moduleIndex
                            , moduleEmit = e
                            }
                    )
            )


initCore : ( ModuleWithCache.ModuleWithResult, List Emit )
initCore =
    ModuleWithCache.init
        { name = Label.make Label.hc [ Label.oo, Label.or, Label.oe ]
        , readMe = "プログラムに最低限必要なものが含まれている標準ライブラリ。足し算引き算、論理演算などの演算や、リスト、辞書、集合などの基本データ構造を含む"
        , typeDefList = []
        , partDefList =
            [ Def.make
                { name = PartDefName.fromLabel (Label.make Label.ha [ Label.ob, Label.os ])
                , type_ = PartDefType.empty
                , expr = Expr.empty
                }
            ]
        }
        |> Tuple.mapSecond (List.map (\moduleEmit -> EmitModule { moduleIndex = SourceIndex.Core, moduleEmit = moduleEmit }))


initCoreInt32 : ( ModuleWithCache.ModuleWithResult, List Emit )
initCoreInt32 =
    ModuleWithCache.init
        { name = Label.make Label.hi [ Label.on, Label.ot, Label.o3, Label.o2 ]
        , readMe = "WebAssemblyでサポートされている32bit符号付き整数を扱えるようになる"
        , typeDefList =
            [ TypeDef.typeDefInt ]
        , partDefList =
            [ Def.make
                { name =
                    PartDefName.fromLabel
                        (Label.make Label.ho
                            [ Label.on, Label.oe, Label.oP, Label.ol, Label.ou, Label.os, Label.oT, Label.ow, Label.oo ]
                        )
                , type_ =
                    PartDefType.Valid
                        (SourceIndex.TypeIndex
                            { moduleIndex = SourceIndex.CoreInt32
                            , typeIndex = ModuleIndex.TypeDefIndex 0
                            }
                        )
                , expr =
                    Expr.make
                        (Expr.Int32Literal 1)
                        [ ( Expr.Add
                          , Expr.Int32Literal 2
                          )
                        ]
                }
            , Def.make
                { name =
                    PartDefName.fromLabel
                        (Label.make Label.ha
                            [ Label.od, Label.od ]
                        )
                , type_ = PartDefType.empty
                , expr =
                    Expr.empty
                }
            ]
        }
        |> Tuple.mapSecond (List.map (\moduleEmit -> EmitModule { moduleIndex = SourceIndex.CoreInt32, moduleEmit = moduleEmit }))


initSampleModule : ( ModuleWithCache.ModuleWithResult, List Emit )
initSampleModule =
    ModuleWithCache.init
        { name = sampleModuleName
        , readMe = ""
        , typeDefList = []
        , partDefList =
            [ Def.make
                { name =
                    PartDefName.fromLabel
                        (Label.make Label.hp
                            [ Label.oo, Label.oi, Label.on, Label.ot ]
                        )
                , type_ = PartDefType.empty
                , expr =
                    Expr.make
                        (Expr.Int32Literal 1)
                        [ ( Expr.Add
                          , Expr.Int32Literal 2
                          )
                        , ( Expr.Add
                          , Expr.Parentheses
                                (Expr.make
                                    (Expr.Int32Literal 3)
                                    [ ( Expr.Add
                                      , Expr.Int32Literal 4
                                      )
                                    , ( Expr.Add
                                      , Expr.Parentheses
                                            (Expr.make
                                                (Expr.Int32Literal 5)
                                                [ ( Expr.Add
                                                  , Expr.Parentheses
                                                        (Expr.make
                                                            (Expr.Parentheses (Expr.make (Expr.Int32Literal 6) []))
                                                            [ ( Expr.Add
                                                              , Expr.Int32Literal 7
                                                              )
                                                            ]
                                                        )
                                                  )
                                                ]
                                            )
                                      )
                                    , ( Expr.Add, Expr.Int32Literal 8 )
                                    ]
                                )
                          )
                        , ( Expr.Add, Expr.Int32Literal 9 )
                        ]
                }
            ]
        }
        |> Tuple.mapSecond (List.map (\moduleEmit -> EmitModule { moduleIndex = SourceIndex.SampleModule, moduleEmit = moduleEmit }))


{-| SampleModule
-}
sampleModuleName : Label.Label
sampleModuleName =
    Label.make
        Label.hs
        [ Label.oa, Label.om, Label.op, Label.ol, Label.oe, Label.oM, Label.oo, Label.od, Label.ou, Label.ol, Label.oe ]


{-| 参照からモジュールを取得する
-}
getModule : SourceIndex.ModuleIndex -> Source -> ModuleWithCache.ModuleWithResult
getModule moduleRef (Source source) =
    case moduleRef of
        SourceIndex.Core ->
            source.core

        SourceIndex.CoreInt32 ->
            source.coreInt32

        SourceIndex.SampleModule ->
            source.sampleModule


{-| 参照からモジュールを設定する
-}
setModule : SourceIndex.ModuleIndex -> ModuleWithCache.ModuleWithResult -> Source -> Source
setModule moduleRef module_ (Source rec) =
    case moduleRef of
        SourceIndex.Core ->
            Source
                { rec | core = module_ }

        SourceIndex.CoreInt32 ->
            Source
                { rec | coreInt32 = module_ }

        SourceIndex.SampleModule ->
            Source
                { rec | sampleModule = module_ }


{-| 参照からモジュールを加工する
-}
mapModule : SourceIndex.ModuleIndex -> (ModuleWithCache.ModuleWithResult -> ModuleWithCache.ModuleWithResult) -> Source -> Source
mapModule moduleRef =
    Utility.Map.toMapper
        (getModule moduleRef)
        (setModule moduleRef)


allModuleRef : Source -> List SourceIndex.ModuleIndex
allModuleRef _ =
    [ SourceIndex.Core
    , SourceIndex.CoreInt32
    , SourceIndex.SampleModule
    ]
