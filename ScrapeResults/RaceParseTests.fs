module RaceParseTests

open System
open NUnit.Framework
open RaceParse

[<TestFixture>]
type RaceListTests() = 
    
    [<Test>]
    member test.TestParseRaces1() = 
        let race = parseRace "event_name=27 for Freedom (BOL)&distance=10km&date=2014-02-08"
        Assert.That(Option.isSome (race), Is.EqualTo(true))
        Assert.That(race.Value.RaceName, Is.EqualTo("27 for Freedom (BOL)"))
        Assert.That(race.Value.RaceDate, Is.EqualTo(new DateTime(2014, 2, 8)))
        Assert.That(race.Value.RaceDistance, Is.EqualTo("10km"))
    
    [<Test>]
    member test.testParseRaces2() = 
        let race = parseRace "event_name=27 for Freedom (BOL)distance=10km&date=2014-02-08"
        Assert.That(Option.isNone (race), Is.EqualTo(true))
    
    [<Test>]
    member test.testGetGender() = 
        Assert.That(getGender ("m"), Is.EqualTo(Some(Gender.Male)))
        Assert.That(getGender ("f"), Is.EqualTo(Some(Gender.Female)))
        Assert.That(getGender ("Ma"), Is.EqualTo(Some(Gender.Male)))
        Assert.That(getGender ("Fe"), Is.EqualTo(Some(Gender.Female)))
        Assert.That(getGender ("bob"), Is.EqualTo(None))
        Assert.That(getGender (""), Is.EqualTo(None))
    
    //getGender
    [<Test>]
    member test.testSomeStr() = 
        Assert.That(someStr ("m"), Is.EqualTo(Some("m")))
        Assert.That(someStr ("1234"), Is.EqualTo(Some("1234")))
        Assert.That(someStr ("1234    "), Is.EqualTo(Some("1234")))
        Assert.That(someStr (""), Is.EqualTo(None))
        Assert.That(someStr ("   "), Is.EqualTo(None))
    
    [<Test>]
    member test.testSomeInt() = 
        Assert.That(someInt ("1"), Is.EqualTo(Some(1)))
        Assert.That(someInt ("01"), Is.EqualTo(Some(1)))
        Assert.That(someInt ("1a"), Is.EqualTo(None))
        Assert.That(someInt (""), Is.EqualTo(None))
    
    [<Test>]
    member test.testSomeDuration() = 
        Assert.That(someDuration ("11:22:33"), Is.EqualTo(Some(11, 22, 33)))
        Assert.That(someDuration ("01:02:03"), Is.EqualTo(Some(1, 2, 3)))
        Assert.That(someDuration ("01a:02:03"), Is.EqualTo(None))
        Assert.That(someDuration ("01:02a:03"), Is.EqualTo(None))
        Assert.That(someDuration ("01:02:"), Is.EqualTo(None))
