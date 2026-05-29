module Juego

open System
open System.IO 
open System.Threading
open System.Text.Json

open App.Utils
open App.Tipos

type ProgramState =
| Running
| Paused of opcionSeleccionada: int
| GameOver
| Terminated

type Misil = { X: int; Y: int }
type EstadoDeSprite = | Vivo | Muerto

type SavedState = {
    AlienX: int
    AlienY: int
    AlienEstado: string // Guardaremos "Vivo" o "Muerto"
    ColisionAlien: int
    Tick: int
    Misiles: Misil list
    EnemigoX: int
    EnemigoY: int
    EnemigoDir: int
    EnemigoEstado: string // Guardaremos "Vivo" o "Muerto"
    ColisionEnemigo: int
    MisilesEnemigos: Misil list
    Vidas: int
    Score: int
}

type State = {
    ProgramState: ProgramState
    AlienX: int
    AlienY: int
    AlienEstado: EstadoDeSprite
    ColisionAlien: int 
    RedibujarPantalla: bool
    Tick: int
    Misiles: Misil list
    EnemigoX: int
    EnemigoY: int
    EnemigoDir: int
    EnemigoEstado: EstadoDeSprite
    ColisionEnemigo: int
    MisilesEnemigos: Misil list
    Vidas: int
    Score: int
}

let estadoInicial = {
    ProgramState = Running
    AlienX = Console.BufferWidth/2
    AlienY = Console.BufferHeight/2
    AlienEstado = Vivo
    ColisionAlien = 0
    RedibujarPantalla = true
    Tick = -1
    Misiles = []
    EnemigoX= Console.BufferWidth-2
    EnemigoY= 0
    EnemigoDir=1
    EnemigoEstado = Vivo
    ColisionEnemigo = 0
    MisilesEnemigos = []
    Vidas = 3
    Score = 0
}

// 
//   FUNCIONES DE GUARDAR PARTIDA
//

let rutaArchivo = "partida.json"

let guardarPartidaEnJson (state: State) =
    try
        //  Mapeo funcional de tipos. Transformamos el estado interactivo 
        // a nuestra estructura plana compatible con JSON.
        let saved = {
            AlienX = state.AlienX
            AlienY = state.AlienY
            AlienEstado = match state.AlienEstado with Vivo -> "Vivo" | Muerto -> "Muerto"
            ColisionAlien = state.ColisionAlien
            Tick = state.Tick
            Misiles = state.Misiles
            EnemigoX = state.EnemigoX
            EnemigoY = state.EnemigoY
            EnemigoDir = state.EnemigoDir
            EnemigoEstado = match state.EnemigoEstado with Vivo -> "Vivo" | Muerto -> "Muerto"
            ColisionEnemigo = state.ColisionEnemigo
            MisilesEnemigos = state.MisilesEnemigos
            Vidas = state.Vidas
            Score = state.Score
        }
        
        let jsonString = JsonSerializer.Serialize(saved)
        File.WriteAllText(rutaArchivo, jsonString)
    with 
    | _ -> () 

let cargarPartidaDesdeJson () =
    if File.Exists(rutaArchivo) then
        try
            let jsonString = File.ReadAllText(rutaArchivo)
            let saved = JsonSerializer.Deserialize<SavedState>(jsonString)
            
            let estadoReconstruido = {
                ProgramState = Paused 0 // Forzamos a que inicie pausado
                AlienX = saved.AlienX
                AlienY = saved.AlienY
                AlienEstado = match saved.AlienEstado with "Muerto" -> Muerto | _ -> Vivo
                ColisionAlien = saved.ColisionAlien
                RedibujarPantalla = true
                Tick = saved.Tick
                Misiles = saved.Misiles
                EnemigoX = saved.EnemigoX
                EnemigoY = saved.EnemigoY // Mapea la propiedad Y guardada
                EnemigoDir = saved.EnemigoDir
                EnemigoEstado = match saved.EnemigoEstado with "Muerto" -> Muerto | _ -> Vivo
                ColisionEnemigo = saved.ColisionEnemigo
                MisilesEnemigos = saved.MisilesEnemigos
                Vidas = saved.Vidas
                Score = saved.Score
            }
            Some estadoReconstruido
        with
        | _ -> None
    else
        None

// 
//   LOGICA DEL JUEGO
// 

let actualizarTick state =
    match state.ProgramState with
    | Running -> { state with Tick = state.Tick + 1 }
    | _ -> state 

let actualizarMisiles state =
    if state.ProgramState = Running && state.Misiles <> [] then 
        state.Misiles
        |> Seq.map (fun misil -> {misil with X = misil.X+1})
        |> Seq.filter (fun misil -> misil.X < Console.BufferWidth-2)
        |> Seq.toList
        |> fun nuevosMisiles -> {state with Misiles = nuevosMisiles; RedibujarPantalla=true}
    else state

let actualizarMisilesEnemigos state =
    if state.ProgramState = Running && state.MisilesEnemigos <> [] then 
        state.MisilesEnemigos
        |> Seq.map (fun misil -> {misil with X = misil.X-1})
        |> Seq.filter (fun misil -> misil.X >= 0)
        |> Seq.toList
        |> fun nuevosMisiles -> {state with MisilesEnemigos = nuevosMisiles; RedibujarPantalla=true}
    else state

let actualizarEnemigo state =
    if state.ProgramState = Running && state.EnemigoEstado = Vivo && state.Tick % 4 = 0 then 
        let nuevoY = state.EnemigoY + state.EnemigoDir
        let nuevaDir,Y = 
            match nuevoY with 
            | y when y > Console.BufferHeight-1 -> -1,Console.BufferHeight-1
            | y when y < 0 -> 1,0
            | _ -> state.EnemigoDir,nuevoY
        {state with EnemigoY = Y; EnemigoDir=nuevaDir; RedibujarPantalla=true}
    else state

let dispararMisilesEnemigos state =
    //  Validación de precondición estricta.
    // El enemigo SOLO puede registrar nuevos misiles en el pipeline si su estado es exactamente 'Vivo'.
    if state.ProgramState = Running && state.EnemigoEstado = Vivo && state.Tick % 10 = 0 then 
        let nuevoMisil = { X = state.EnemigoX-2; Y = state.EnemigoY }
        {state with MisilesEnemigos = nuevoMisil :: state.MisilesEnemigos; RedibujarPantalla = true}
    else
        state

let detectarColisionAlien state =
    if state.ProgramState = Running then
        state.MisilesEnemigos
        |> List.filter ( fun misil -> not ( misil.Y = state.AlienY && misil.X = state.AlienX+1))
        |> fun nuevosMisiles ->
            if nuevosMisiles.Length <> state.MisilesEnemigos.Length then
                let nuevasVidas = state.Vidas - 1
                let nuevoEstadoPrograma = if nuevasVidas <= 0 then GameOver else Running
                {state with 
                    AlienEstado = Muerto; MisilesEnemigos = nuevosMisiles
                    RedibujarPantalla = true; ColisionAlien = state.Tick
                    Vidas = nuevasVidas; ProgramState = nuevoEstadoPrograma}
            else state 
    else state

let detectarColisionEnemigo state =
    if state.ProgramState = Running then
        state.Misiles
        |> List.filter ( fun misil -> not ( misil.Y = state.EnemigoY && misil.X = state.EnemigoX-1))
        |> fun nuevosMisiles ->
            // Si la longitud de las listas difiere, significa que un misil golpeó las coordenadas del enemigo
            if nuevosMisiles.Length <> state.Misiles.Length then
                if state.EnemigoEstado = Vivo then
                    // PRIMER IMPACTO: El enemigo estaba vivo. Lo matamos y sumamos 1 punto de forma legítima.
                    {state with 
                        EnemigoEstado = Muerto
                        Misiles = nuevosMisiles
                        RedibujarPantalla = true
                        ColisionEnemigo = state.Tick
                        Score = state.Score + 1}
                else
                    // IMPACTOS SUBSECUENTES: El enemigo ya estaba explotando (Muerto). 
                    // Limpiamos el misil del mapa para mantener la estética, pero NO sumamos puntos.
                    {state with 
                        Misiles = nuevosMisiles
                        RedibujarPantalla = true}
            else state 
    else state

let resetAlien state =
    if state.ProgramState = Running && state.AlienEstado = Muerto && state.Vidas > 0 then 
        let tiempo = state.Tick-state.ColisionAlien
        if tiempo >= 120 then {state with AlienEstado=Vivo; RedibujarPantalla=true}
        else state
    else state

let resetEnemigo state =
    if state.ProgramState = Running && state.EnemigoEstado = Muerto then 
        let tiempo = state.Tick-state.ColisionEnemigo
        if tiempo >= 120 then {state with EnemigoEstado=Vivo; RedibujarPantalla=true}
        else state
    else state

//
//  Rediseño completo de la lectura de teclado.
// Evaluamos de forma diferente si el juego está activo o en el sub-menú de pausa.
//
let procesarTecladoGlobal key state =
    match state.ProgramState with
    | Running ->
        match key with 
        | ConsoleKey.P -> { state with ProgramState = Paused 0; RedibujarPantalla = true }
        | ConsoleKey.Escape -> { state with ProgramState = Terminated }
        | _ -> state

    | Paused opcion ->
        match key with
        | ConsoleKey.UpArrow -> { state with ProgramState = Paused (max 0 (opcion - 1)); RedibujarPantalla = true }
        | ConsoleKey.DownArrow -> { state with ProgramState = Paused (min 2 (opcion + 1)); RedibujarPantalla = true }
        | ConsoleKey.Enter ->
            match opcion with
            | 0 -> { state with ProgramState = Running; RedibujarPantalla = true } // Continuar
            | 1 -> 
                guardarPartidaEnJson state // Guardar Partida en archivo JSON

                Console.Clear()
                mostrarMensaje ((Console.BufferWidth / 2) - 8) (Console.BufferHeight / 2) ConsoleColor.Green "¡PARTIDA GUARDADA!"
                Thread.Sleep(1000)


                { state with ProgramState = Terminated } 
            | _ ->
                 { state with ProgramState = Terminated } // Salir al Menú Principal
        | _ -> state
    |GameOver ->
        // Añadimos el estado GameOver al procesador de teclado.
        // Ahora el juego reacciona correctamente a la tecla Escape para romper el bucle recursivo.
        match key with
        |ConsoleKey.Escape -> { state with ProgramState = Terminated }
        | _ -> state
    | _ -> state

let procesarTecladoDeAlien key state =
    if state.ProgramState = Running && state.AlienEstado = Vivo then 
        match key with  
        | ConsoleKey.UpArrow -> {state with AlienY = max 0 (state.AlienY-1)}
        | ConsoleKey.DownArrow -> {state with AlienY = min (Console.BufferHeight-1) (state.AlienY+1)}
        | ConsoleKey.LeftArrow -> {state with AlienX = max 0 (state.AlienX-1)}
        | ConsoleKey.RightArrow -> {state with AlienX = min (Console.BufferWidth-2) (state.AlienX+1)}
        | ConsoleKey.Spacebar ->
            if state.EnemigoEstado = Vivo then
                let nuevoMisil = { X = state.AlienX+2; Y = state.AlienY }
                {state with Misiles = nuevoMisil :: state.Misiles}
            else
                state
        | _ -> state
        |> fun newState -> if newState <> state then {newState with RedibujarPantalla=true} else state
    else state

let procesarTeclado state =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        state 
        |> procesarTecladoGlobal k.Key
        |> procesarTecladoDeAlien k.Key
    else state

// 
//   LOGICA DE ANIMACIONES
// 

let redibujarAlien state =
    if state.ProgramState <> GameOver then
        let sprite = if state.AlienEstado = Vivo then "👽" else "💥"
        mostrarMensaje state.AlienX state.AlienY ConsoleColor.Yellow sprite

let redibujarMisiles state =
    state.Misiles |> List.iter (fun m -> mostrarMensaje m.X m.Y ConsoleColor.Yellow "=>")

let redibujarMisilesEnemigos state =
    state.MisilesEnemigos |> List.iter (fun m -> mostrarMensaje m.X m.Y ConsoleColor.Cyan "<=")

let redibujarEnemigo state =
    if state.ProgramState <> GameOver then
        let sprite = if state.EnemigoEstado = Vivo then "☠️" else "💥"
        mostrarMensaje state.EnemigoX state.EnemigoY ConsoleColor.Yellow sprite

let redibujarContadores state =
    mostrarMensaje 1 0 ConsoleColor.Green (sprintf "VIDAS: %d" state.Vidas)
    let textoScore = sprintf "SCORE: %d" state.Score
    mostrarMensaje (Console.BufferWidth - textoScore.Length - 1) 0 ConsoleColor.Magenta textoScore

//
//  Menú de Pausa
//
let redibujarMenuPausa state =
    match state.ProgramState with
    | Paused opcion ->
        let centroX = (Console.BufferWidth / 2) - 10
        let centroY = Console.BufferHeight / 2 - 2
        
        mostrarMensaje centroX (centroY + 1) ConsoleColor.Red "JUEGO PAUSADO"
        
        let arrow0 = if opcion = 0 then "▶ " else "  "
        mostrarMensaje (centroX + 2) (centroY + 4) ConsoleColor.Green (arrow0 + "Continuar")
        
        let arrow1 = if opcion = 1 then "▶ " else "  "
        mostrarMensaje (centroX + 2) (centroY + 5) ConsoleColor.Green (arrow1 + "Guardar partida")
        
        let arrow2 = if opcion = 2 then "▶ " else "  "
        mostrarMensaje (centroX + 2) (centroY + 6) ConsoleColor.Green (arrow2 + "Salir al menu")
    | _ -> ()

let redibujarGameOver state =
    if state.ProgramState = GameOver then
        let centroX = (Console.BufferWidth / 2) - 7
        let centroY = Console.BufferHeight / 2

        mostrarMensaje centroX (centroY + 1) ConsoleColor.Red "GAME OVER"

        mostrarMensaje (centroX - 4) (centroY + 4) ConsoleColor.Green "Presione ESC para salir"

let redibujarPantalla state =
    if state.RedibujarPantalla then 
        Console.Clear()
        [|
            redibujarMisiles; redibujarAlien; redibujarEnemigo; redibujarMisilesEnemigos
            redibujarContadores; redibujarMenuPausa; redibujarGameOver
        |] |> Array.iter (fun f -> state |> f)
        {state with RedibujarPantalla=false}
    else state

//
// BUCLE PRINCIPAL RECURSIVO DEL ARCHIVO JUEGO
//
let rec mainLoop state =
    let newState =
        state 
        |> actualizarTick |> actualizarMisiles |> actualizarEnemigo 
        |> dispararMisilesEnemigos |> actualizarMisilesEnemigos 
        |> detectarColisionAlien |> detectarColisionEnemigo 
        |> resetAlien |> resetEnemigo |> procesarTeclado |> redibujarPantalla
        
    if newState.ProgramState <> Terminated then 
        Thread.Sleep 25
        newState |> mainLoop

//  Función expuesta para inicializar una nueva partida limpia desde cero
let comenzar () =
    Console.Clear()
    Console.CursorVisible <- false
    estadoInicial |> mainLoop

    Console.ResetColor()       // Devuelve el color de texto y fondo por defecto de la terminal
    Console.Clear()            // Limpia los restos visuales del mapa del juego
    Console.CursorVisible <- true // Devuelve el cursor parpadeante al usuario

//  Función expuesta para reanudar el estado que recuperamos de JSON
let cargarPartida (estadoRecuperado: State) =
    Console.Clear()
    Console.CursorVisible <- false
    estadoRecuperado |> mainLoop


    Console.ResetColor()       // Devuelve el color de texto y fondo por defecto de la terminal
    Console.Clear()            // Limpia los restos visuales del mapa del juego
    Console.CursorVisible <- true // Devuelve el cursor parpadeante al usuario