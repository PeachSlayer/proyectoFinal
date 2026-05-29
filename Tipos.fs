module App.Tipos
//
// Unión Discriminada que representa las acciones posibles que el menú
// principal puede devolverle al Enrutador una vez que el usuario elige.
// 
type Comando =
    | NuevoJuego
    | CargarJuego
    | Salir
