module Feliz.ReactResizable

open Fable.Core.JsInterop
open Feliz

importSideEffects "react-resizable/css/styles.css"
let resizableBox: obj = import "ResizableBox" "react-resizable"


type ResizeHandleAxis =
    | North
    | South
    | West
    | East
    | NorthWest
    | NorthEast
    | SouthWest
    | SouthEast

    static member Value = function
        | South -> "s"
        | East -> "e"
        | North -> "n"
        | West -> "w"
        | NorthWest -> "nw"
        | NorthEast -> "ne"
        | SouthWest -> "sw"
        | SouthEast -> "se"

type IResizableProp = interface end

let (=>) key value = unbox<IResizableProp>(key ==> value)

type Axis =
    | X
    | Y
    | Both

    static member Value = function
        | X -> "x"
        | Y -> "y"
        | Both -> "both"

type ResizableBox =

    static member inline Width (number: float) =  "width" => number
    static member inline Height (number: float) = "height" => number

    static member HandleSize (num1: float, num2: float) = "handleSize" => [| num1; num2 |]

    static member Axis (axis: Axis option) =
        let value =
            match axis with
            | None -> "none"
            | Some x -> Axis.Value x

        "axis" => value

    static member MinConstraints (num1: float, num2: float) = "minConstraints" => [| num1; num2 |]

    static member MaxConstraints (num1: float, num2: float) = "maxConstraints" => [| num1; num2 |]

    static member LockAspectRatio (bool: bool) = "lockAspectRatio" => bool

    static member ResizeHandles (handles: ResizeHandleAxis list) =
        let handleAxis =
            handles
            |> List.map ResizeHandleAxis.Value
            |> List.toArray
        "resizeHandles" => handleAxis

    static member inline Children (e: ReactElement) = unbox<IResizableProp>(prop.children e)
    static member inline Create props = Interop.reactApi.createElement (resizableBox, createObj !!props)