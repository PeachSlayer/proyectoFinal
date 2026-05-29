module App.Menu

open System
open App.Utils
open App.Tipos

let rec private loopMenu opcionSeleccionada =
    Console.Clear()
    
    // Calcular el centro de la pantalla para estética arcade
    let centroX = (Console.BufferWidth / 2) - 8
    let centroY = (Console.BufferHeight / 2) - 2
    
    mostrarMensaje centroX (centroY - 2) ConsoleColor.Green "==ALIEN X 🛸==="

    // Dibujar las opciones. Evalu si el índice actual coincide con la opción seleccionada.
    let arrow0 = if opcionSeleccionada = 0 then "▶ " else "  "
    mostrarMensaje centroX centroY ConsoleColor.Green (arrow0 + "Nuevo juego")

    let arrow1 = if opcionSeleccionada = 1 then "▶ " else "  "
    mostrarMensaje centroX (centroY + 1) ConsoleColor.Green (arrow1 + "Cargar juego")

    let arrow2 = if opcionSeleccionada = 2 then "▶ " else "  "
    mostrarMensaje centroX (centroY + 2) ConsoleColor.Green (arrow2 + "Salir")

    
    let tecla = Console.ReadKey(true).Key

    match tecla with
    | ConsoleKey.UpArrow -> 
        // max 0 asegura que el cursor no baje de la opción 0
        loopMenu (max 0 (opcionSeleccionada - 1))
        
    | ConsoleKey.DownArrow -> 
        // min 2 asegura que el cursor no suba de la opción 2
        loopMenu (min 2 (opcionSeleccionada + 1))
        
    | ConsoleKey.Enter -> 
        // Al presionar Enter, transformamos el índice entero en un tipo fuertemente tipado 'Comando'
        match opcionSeleccionada with
        | 0 -> NuevoJuego
        | 1 -> CargarJuego
        | _ -> Salir
        
    | _ -> 
        // Si presionan cualquier otra tecla, el menú se redibuja en la misma posición
        loopMenu opcionSeleccionada

let mostrar () =
    Console.CursorVisible <- false
    loopMenu 0 // Iniciamos el menú apuntando a la primera opción (índice 0)