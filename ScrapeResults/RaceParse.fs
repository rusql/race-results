
#if INTERACTIVE
#r "D:/Dev/lib/fsharp/FSharp.Data/bin/portable7/FSharp.Data.dll"
#r "System.Data.Entity.dll"
#r "FSharp.Data.TypeProviders.dll"
#r "System.Data.Linq.dll"
#else
module RaceParse
#endif


open System
open System.Data.Linq
open System.Data.EntityClient
open Microsoft.FSharp.Data.TypeProviders
open System.IO
open FSharp.Data
open System.Text.RegularExpressions
open System.Text

type Race = 
    { RaceName : string
      RaceDate : DateTime
      RaceDistance : string
      RaceParam : string }

type Gender = | Male | Female

type RaceResult = 
    { Position : int
      Initials : string option
      Surname : string option
      Sex : Gender option
      Age : int option
      Club : string 
      FinishTime : int option}// Finish time stored in seconds. Optional in case we cannot parse it from the data

let raceListURL year = "http://www.raceresults.co.za/club_selection_page.php?year=" + year.ToString()
let raceListPage year = HtmlDocument.Load(raceListURL year)


// Tries to parse a race from text in the form : event_name=[EVENT]&distance=[DISTANCE]&date=[DATE]
// eg. event_name=Ace&distance=15Km&date=2001-10-20
// Returns Some (race) if successfully parsed, otherwise None.
let parseRace raceOptionText = 
    let pattern = @"event_name=(.+)&distance=(.+)&date=(.+)"
    let raceMatch : Match = Regex.Match(raceOptionText, pattern)
    // Get the race details
    let thisName = raceMatch.Groups.Item(1).ToString()
    let thisDistance = raceMatch.Groups.Item(2).ToString()
    let dateOk, thisDate = DateTime.TryParse(raceMatch.Groups.Item(3).ToString())
    // Check that we have all the pieces
    if String.IsNullOrWhiteSpace(thisName) || String.IsNullOrWhiteSpace(thisDistance) || not dateOk then None // Cannot put Humpty together...
    else 
        let race : Race = 
            { RaceName = thisName
              RaceDistance = thisDistance
              RaceDate = thisDate
              RaceParam = raceOptionText }
        Some(race)


// Return Seq<race> : a Sequence of sucessfully parsed races from an HTML Document
// Seq.choose filters out all None values
let races year = 
    (raceListPage year).Descendants [ "option" ] |> Seq.choose (fun x -> 
                                                        x.TryGetAttribute("value")
                                                        |> Option.map (fun o -> o.Value())
                                                        //|> Option.map (fun r -> parseRace r))
                                                        |> Option.map (parseRace))



// HTML Document of results for a race
let raceText = 
    Http.RequestString(@"http://www.raceresults.co.za/output_clubs.php", 
                       body = FormValues [ ("search_club", "Pirates")
                                           ("events", 
                                            @"event_name=Chamberlain Capital Classic (AGN)&distance=21km&date=2014-10-11")
                                           ("C1", @"Go get it") ])

// HTML string of results for a race
// eventName eventDistance eventDate are the required HTML Post parameters in string representation
// Eg. RaceText : "Chamberlain Capital Classic (AGN)", eventDistance : "21km", eventDate : "2014-10-11"
// Eg. FullRaceText "Chamberlain Capital Classic (AGN)" "21km" "2014-10-11"

let InitialFullRaceText eventName eventDistance eventDate= 
    Http.RequestString(@"http://www.raceresults.co.za/output_full_results.php", 
                       body = FormValues [ ("sex", "Select your gender")
                                           ("events", @"event_name="+eventName+"&distance="+eventDistance+"&date="+eventDate)
                                           ("C1", @"Go get it") ])


let NextFullRaceText eventName eventDistance eventDate startAtPos= 
    Http.RequestString(@"http://www.raceresults.co.za/output_full_results.php?max="+startAtPos+"&event_name="+eventName+"&date="+eventDate+"&distance="+eventDistance+"&sex=Select%20your%20gender&not=1")


let raceDocument = HtmlDocument.Load(new MemoryStream (Encoding.UTF8.GetBytes(raceText)))

/// Returns a subset of a list from the first occurrence of a given item
let rec chopTo item l  = 
  match l with
  | hd::tl -> if hd = item then tl else chopTo item tl 
  | _ -> []


  
let raceResults = 
    raceDocument.Descendants [ "td" ] 
    |> Seq.map (fun x -> x.InnerText())
    |> Seq.toList
    |> chopTo "Finish Time"

let parseRaceResults textResults = 
    let rec parseRaceResultsHelper lst  acc=
        match lst with
        | pos :: initial :: surname :: sex :: age :: club :: finishtime :: tail -> parseRaceResultsHelper tail ((pos, initial, surname, sex, age, club, finishtime)::acc)
        | _ -> acc
    parseRaceResultsHelper textResults []

/// Determine the gender from a string
let getGender g = 
    let s = g.ToString().ToUpper()
    if s.StartsWith("M") 
        then Some(Gender.Male)
    else if s.StartsWith("F") 
        then Some(Gender.Female)
    else None

let someStr s = 
  if (s.ToString().Trim() <> "") then Some (s.ToString().Trim()) else None

let someInt i = 
  let intOk, intValue = Int32.TryParse(i)
  if intOk then Some (intValue) else None

let someDuration d = 
    let m : Match = Regex.Match(d, @"(\d*):(\d*):(\d*)")
    let thisHr = someInt (m.Groups.Item(1).ToString())
    let thisMin = someInt (m.Groups.Item(2).ToString())
    let thisSec = someInt (m.Groups.Item(3).ToString())
    match (thisHr, thisMin, thisSec) with
    | (Some (h), Some (m), Some (s)) -> Some (h, m, s)
    | _ -> None

let getResult pos initials surname sex age club duration = 
    (someInt(pos), someStr(initials), someStr(surname), someStr(sex), someInt(age), someStr(club), someDuration(duration))


//type private EntityConnection = SqlEntityConnection<ConnectionString="Server=localhost\sql2012;Initial Catalog=RunningResults;Integrated Security=SSPI;MultipleActiveResultSets=true", Pluralize = false>

//let getClubs = 
//    let context = EntityConnection.GetDataContext()
//    query { for c in context.Club do
//            select c}
//    |> Seq.iter (fun c -> printfn "%s" c.ClubName)

// All the years from 1998 to now
let YearsAvailable = [for i in 2015..System.DateTime.Now.Year -> i]

let RacesAvailable = 
    YearsAvailable 
//    |> List.map (fun r -> races r)
    |> List.map (races)



// FullRaceDocument "Chamberlain Capital Classic (AGN)" "21km" "2014-10-11"
let InitialFullRaceDocument eventName eventDistance eventDate = 
    let fullRaceHtml = InitialFullRaceText eventName eventDistance eventDate
    HtmlDocument.Load(new MemoryStream (Encoding.UTF8.GetBytes(fullRaceHtml)))





let NextRaceDocumentStartPos doc = 
//    let pattern = @"(<a href='output_full_results\.php\?max=)(\d+)(&event_name=)(.+)(&date=)(2014-10-11)(&distance=)(.+)(&sex=)(.+)(Next</a>)"
    let pattern = @"(<a href.+\?max=)(\d+)(&event_name=)(.+)(&date=)(2014-10-11)(&distance=)(.+)(&sex=)(.+)(Next</a>)"
    let m : Match = Regex.Match(doc.ToString(), pattern) 
    printf "%b\n" m.Success
    let s = m.Success
    match s with
    | true -> 
        if (m.Groups.Count >= 3) then (string)m.Groups.[2] |> Some  else  None
    | false -> None



NextRaceDocumentStartPos (InitialFullRaceDocument "Chamberlain Capital Classic (AGN)" "21km" "2014-10-11")


  
let d = InitialFullRaceDocument "Chamberlain Capital Classic (AGN)" "21km" "2014-10-11"

NextRaceDocumentStartPos d


let d = NextFullRaceText "Chamberlain Capital Classic (AGN)" "21km" "2014-10-11" "400"



//let cclasic = FullRaceDocument "Chamberlain Capital Classic (AGN)" "21km" "2014-10-11"





// Uri.EscapeDataString "ss ss";;


