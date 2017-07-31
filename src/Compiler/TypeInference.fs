// type Type =
//         {
//             Name : string;
//             Guid : System.Guid;
//             TypeParameters : Type list;
//             ImplementedInterfaces : Type list;
//             BaseClass : Type
//         }

// let createType name typeParams : Type= {Name=name; Guid = System.Guid.NewGuid(); TypeParameters = typeParams}
// let createParameterlessType name : Type = {Name = name; Guid = System.Guid.NewGuid(); TypeParameters = []}


// createType "Dictionary" [(createParameterlessType "int" ); (createParameterlessType "string" )];;