open System
open System.Text.RegularExpressions

type DrivingLicense(series: string, number: string, ownerName: string) =
    do if not (Regex.IsMatch(series, @"^\d{4}$")) then
        raise (ArgumentException("Invalid series format (must be 4 digits)"))
    
    do if not (Regex.IsMatch(number, @"^\d{6}$")) then
        raise (ArgumentException("Invalid number format (must be 6 digits)"))
    
    member _.Series = series
    member _.Number = number
    member _.OwnerName = ownerName

    override this.ToString() =
        $"Driving License: Series {this.Series}, Number {this.Number}, Owner: {this.OwnerName}"

    interface IEquatable<DrivingLicense> with
        member this.Equals(other) =
            this.Series = other.Series && this.Number = other.Number

    override this.Equals(obj) =
        match obj with
        | :? DrivingLicense as other -> (this :> IEquatable<DrivingLicense>).Equals(other)
        | _ -> false

    override this.GetHashCode() =
        hash (this.Series, this.Number)

let tryCreateLicense() =
    try
        let license1 = DrivingLicense("1234", "567890", "John Doe")
        let license2 = DrivingLicense("1234", "567890", "Jane Smith")
        
        printfn "%O" license1
        printfn "%O" license2
        
        printfn $"Equal: {license1 = license2}" 
        
        let invalidLicense = DrivingLicense("12A4", "123", "Invalid")
        invalidLicense
    with
    | :? ArgumentException as ex ->
        printfn $"Validation error: {ex.Message}"
        reraise()

tryCreateLicense()