// Módulo Enrutador: orquestador central de la aplicación.
// Decide qué pantalla mostrar en cada momento y transiciona entre ellas
// dependiendo de los resultados que devuelven el Menú y el Juego.
// Es el único módulo que "conoce" a todos los demás; ningún otro módulo
// lo referencia a él, lo que mantiene las dependencias en una sola dirección.
module App.Enrutador

open App.Tipos
open System

type EstadosEnrutador =
    | MostrarMenu
    | MostrarJuego
    | MostrarCargarJuego
    | Terminar


let estadoInicial = MostrarMenu

// Bucle recursivo principal de la aplicación.
// Cada iteración toma el estado actual, ejecuta la pantalla correspondiente
// y calcula el estado siguiente. La recursión se detiene cuando el estado
// llega a 'Terminar', evitando así la necesidad de bucles while con mutables.
let rec loopPrincipal estado =
    let nuevoEstado =
        match estado with

        | MostrarMenu ->
            // Mostramos el menú y mapeamos el Comando devuelto
            // al siguiente estado del enrutador.
            match App.Menu.mostrar() with
            | NuevoJuego  -> MostrarJuego
            | CargarJuego -> MostrarCargarJuego
            | Salir       -> Terminar

        | MostrarJuego ->
            // Iniciamos una partida completamente nueva desde el estado inicial.
            // Cuando mainLoop termina (el usuario sale o muere), volvemos al menú.
            Juego.comenzar()
            MostrarMenu

        | MostrarCargarJuego ->
            // Intentamos deserializar el archivo partida.json.
            // cargarPartidaDesdeJson devuelve Option<State>:
            //   Some estado → archivo válido; inyectamos ese estado al juego
            //   None        → no existe el archivo o está corrupto; avisamos y volvemos
            match Juego.cargarPartidaDesdeJson() with
            | Some estadoGuardado ->
                Juego.cargarPartida estadoGuardado
                MostrarMenu
            | None ->
                Console.Clear()
                App.Utils.mostrarMensaje 5 (Console.BufferHeight / 2) ConsoleColor.Red "¡No existe ninguna partida guardada!"
                System.Threading.Thread.Sleep(1200)
                MostrarMenu

        | Terminar ->
            // Estado terminal: nos retornamos a nosotros mismos para
            // que la guarda de abajo detenga la recursión.
            Terminar

    // Continuamos el bucle solo si no hemos llegado al estado final.
    if nuevoEstado <> Terminar then
        loopPrincipal nuevoEstado

// Punto de entrada público: inicializa el enrutador y, al terminar,
// restaura la terminal al estado limpio que tenía antes del juego.
let mostrar () =
    estadoInicial |> loopPrincipal

    // Restauramos el entorno de la terminal para el usuario
    Console.ResetColor()        // Devuelve colores de texto y fondo por defecto
    Console.Clear()             // Elimina cualquier rastro visual del juego
    Console.CursorVisible <- true  // Vuelve a mostrar el cursor parpadeante
