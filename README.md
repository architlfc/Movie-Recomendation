# Movie-Recomendationrecommendation-col
================
Archit
21 November 2018

``` r
train_data<-read.csv("C:/Users/Administrator/Desktop/machine learning/train_v2.csv")
head(train_data)
```

    ##       ID user movie rating
    ## 1 610739 3704  3784      3
    ## 2 324753 1924   802      3
    ## 3 808218 4837  1387      4
    ## 4 133808  867  1196      4
    ## 5 431858 2631  3072      5
    ## 6 895320 5410  2049      4

``` r
train_data<-train_data[,2:4]#-c(1)

summary(train_data)
```

    ##       user          movie          rating    
    ##  Min.   :   6   Min.   :   1   Min.   :1.00  
    ##  1st Qu.:1492   1st Qu.:1004   1st Qu.:3.00  
    ##  Median :3089   Median :1860   Median :4.00  
    ##  Mean   :3029   Mean   :1858   Mean   :3.59  
    ##  3rd Qu.:4486   3rd Qu.:2761   3rd Qu.:4.00  
    ##  Max.   :6037   Max.   :3952   Max.   :5.00

``` r
#install.packages("reshape2")
library(reshape2)

ratingsmatrix<-acast(train_data,user~movie)
View(ratingsmatrix)

class(ratingsmatrix)
```

    ## [1] "matrix"

``` r
#if not a matrix we would hv to convert it into one

#convert the matrix into a real rating matrix
#real rating matrix is a sparse matrix like ds 
#install.packages("arules")
library(arules)
#install.packages("recommenderlab")
library(recommenderlab)

rratingsmatrix<-as(ratingsmatrix,"realRatingMatrix")
rratingsmatrix
```

    ## 1342 x 1095 rating matrix of class 'realRatingMatrix' with 1810 ratings.

``` r
#a view in other possible ways
#as(rratingsmatrix,"list")
#as(rratingsmatrix,"matrix")

#pART DEUX

data("MovieLense")
MovieLense
```

    ## 943 x 1664 rating matrix of class 'realRatingMatrix' with 99392 ratings.

``` r
sample_MovieLense<-sample.int(nrow(MovieLense),0.999*nrow(MovieLense))
train_MovieLense<-MovieLense[sample_MovieLense]
test_MovieLense<-MovieLense[-sample_MovieLense]

#build the model using user based collabrate fiilerting
recom_mode_ML1<-Recommender(train_MovieLense,method="UBCF")
recom_mode_ML2<-Recommender(train_MovieLense,method="SVD")

#predict 
predict_ML1<-predict(recom_mode_ML1,test_MovieLense,n=3)
predict_ML2<-predict(recom_mode_ML2,test_MovieLense,n=3)


as(predict_ML1,"list")
```

    ## $`373`
    ## [1] "Titanic (1997)"           "L.A. Confidential (1997)"
    ## [3] "Contact (1997)"

``` r
as(predict_ML2,"list")
```

    ## $`373`
    ## [1] "Liar Liar (1997)" "Contact (1997)"   "Toy Story (1995)"

``` r
as(test_MovieLense,"list")
```

    ## $`373`
    ##                                                            GoldenEye (1995) 
    ##                                                                           4 
    ##                                                           Get Shorty (1995) 
    ##                                                                           4 
    ##                                                  Usual Suspects, The (1995) 
    ##                                                                           5 
    ##                                                   Mr. Holland's Opus (1995) 
    ##                                                                           4 
    ##                                                   Angels and Insects (1995) 
    ##                                                                           2 
    ##                                                           Braveheart (1995) 
    ##                                                                           5 
    ##                                                  Rumble in the Bronx (1995) 
    ##                                                                           4 
    ##                                                        Birdcage, The (1996) 
    ##                                                                           4 
    ##                                                            Apollo 13 (1995) 
    ##                                                                           3 
    ##                                                         Crimson Tide (1995) 
    ##                                                                           3 
    ##                                                          Hoop Dreams (1994) 
    ##                                                                           5 
    ##                                                            Star Wars (1977) 
    ##                                                                           5 
    ##                                                            Quiz Show (1994) 
    ##                                                                           4 
    ##                                            Shawshank Redemption, The (1994) 
    ##                                                                           4 
    ##                                              While You Were Sleeping (1995) 
    ##                                                                           4 
    ##                                                            Crow, The (1994) 
    ##                                                                           5 
    ##                                                         Forrest Gump (1994) 
    ##                                                                           4 
    ##                                          Four Weddings and a Funeral (1994) 
    ##                                                                           4 
    ##                                                       Lion King, The (1994) 
    ##                                                                           5 
    ##                                                        Fugitive, The (1993) 
    ##                                                                           4 
    ##                                                 Hot Shots! Part Deux (1993) 
    ##                                                                           3 
    ##                                                 Hudsucker Proxy, The (1994) 
    ##                                                                           2 
    ##                                                        Jurassic Park (1993) 
    ##                                                                           1 
    ##                                               Much Ado About Nothing (1993) 
    ##                                                                           5 
    ##                                                 Sleepless in Seattle (1993) 
    ##                                                                           4 
    ##                                                         Blade Runner (1982) 
    ##                                                                           5 
    ##                                         So I Married an Axe Murderer (1993) 
    ##                                                                           4 
    ##                                                           Home Alone (1990) 
    ##                                                                           2 
    ##                                                              Aladdin (1992) 
    ##                                                                           5 
    ##                                           Terminator 2: Judgment Day (1991) 
    ##                                                                           4 
    ##                                                   Dances with Wolves (1990) 
    ##                                                                           3 
    ##                                      Snow White and the Seven Dwarfs (1937) 
    ##                                                                           5 
    ##                                                                Fargo (1996) 
    ##                                                                           3 
    ##                                                      Aristocats, The (1970) 
    ##                                                                           5 
    ##                                                           Sgt. Bilko (1996) 
    ##                                                                           3 
    ##                                                 Operation Dumbo Drop (1995) 
    ##                                                                           3 
    ##                      Wallace & Gromit: The Best of Aardman Animation (1996) 
    ##                                                                           5 
    ##                                                            Rock, The (1996) 
    ##                                                                           4 
    ##                                                           Phenomenon (1996) 
    ##                                                                           4 
    ##                                                       Godfather, The (1972) 
    ##                                                                           2 
    ##                                               Breakfast at Tiffany's (1961) 
    ##                                                                           4 
    ##                                                    Wizard of Oz, The (1939) 
    ##                                                                           3 
    ##                                                2001: A Space Odyssey (1968) 
    ##                                                                           1 
    ##                                         Mr. Smith Goes to Washington (1939) 
    ##                                                                           4 
    ##                                                        Love Bug, The (1969) 
    ##                                                                           3 
    ##                                             Bedknobs and Broomsticks (1971) 
    ##                                                                           3 
    ##                                                  Sound of Music, The (1965) 
    ##                                                                           3 
    ##                                                             Die Hard (1988) 
    ##                                                                           3 
    ##                                                             Swingers (1996) 
    ##                                                                           4 
    ##                                Willy Wonka and the Chocolate Factory (1971) 
    ##                                                                           4 
    ##                                                 Fish Called Wanda, A (1988) 
    ##                                                                           5 
    ##                                         Monty Python's Life of Brian (1979) 
    ##                                                                           5 
    ##                                                        Dirty Dancing (1987) 
    ##                                                                           4 
    ##                                                       Reservoir Dogs (1992) 
    ##                                                                           2 
    ##                                                              Top Gun (1986) 
    ##                                                                           4 
    ##                                                       On Golden Pond (1981) 
    ##                                                                           3 
    ##                                      Return of the Pink Panther, The (1974) 
    ##                                                                           4 
    ##                                                     Jean de Florette (1986) 
    ##                                                                           5 
    ##                              Manon of the Spring (Manon des sources) (1986) 
    ##                                                                           5 
    ##                                      Monty Python and the Holy Grail (1974) 
    ##                                                                           5 
    ##                                                  Wrong Trousers, The (1993) 
    ##                                                                           5 
    ##                                                      Cinema Paradiso (1988) 
    ##                                                                           5 
    ##                                             Empire Strikes Back, The (1980) 
    ##                                                                           5 
    ##                                                  Princess Bride, The (1987) 
    ##                                                                           5 
    ##                                              Raiders of the Lost Ark (1981) 
    ##                                                                           4 
    ##                                                               Brazil (1985) 
    ##                                                                           3 
    ##                                      Good, The Bad and The Ugly, The (1966) 
    ##                                                                           3 
    ##                                                         12 Angry Men (1957) 
    ##                                                                           4 
    ##                                                  Clockwork Orange, A (1971) 
    ##                                                                           3 
    ##                                                       Apocalypse Now (1979) 
    ##                                                                           3 
    ##                                                   Return of the Jedi (1983) 
    ##                                                                           5 
    ##                                                     Army of Darkness (1993) 
    ##                                                                           4 
    ##                                                  Blues Brothers, The (1980) 
    ##                                                                           5 
    ##                                              Godfather: Part II, The (1974) 
    ##                                                                           2 
    ##                                                     Grand Day Out, A (1992) 
    ##                                                                           5 
    ##                                                              Henry V (1989) 
    ##                                                                           5 
    ##                                                              Amadeus (1984) 
    ##                                                                           4 
    ##                                                           Sting, The (1973) 
    ##                                                                           4 
    ##                                                      Terminator, The (1984) 
    ##                                                                           4 
    ##                                                   Dead Poets Society (1989) 
    ##                                                                           5 
    ##                                                        Graduate, The (1967) 
    ##                                                                           3 
    ##                                                        Groundhog Day (1993) 
    ##                                                                           3 
    ##                                                   Back to the Future (1985) 
    ##                                                                           5 
    ##                                                                Akira (1988) 
    ##                                                                           4 
    ##                                                   Young Frankenstein (1974) 
    ##                                                                           4 
    ##                                                   This Is Spinal Tap (1984) 
    ##                                                                           4 
    ##                                   Indiana Jones and the Last Crusade (1989) 
    ##                                                                           5 
    ##                                                              M*A*S*H (1970) 
    ##                                                                           4 
    ##                                                  Room with a View, A (1986) 
    ##                                                                           4 
    ##                                                Pink Floyd - The Wall (1982) 
    ##                                                                           4 
    ##                                                      Field of Dreams (1989) 
    ##                                                                           4 
    ##                                              When Harry Met Sally... (1989) 
    ##                                                                           4 
    ##                                                Bram Stoker's Dracula (1992) 
    ##                                                                           3 
    ##                                                       101 Dalmatians (1996) 
    ##                                                                           4 
    ##                                                           Die Hard 2 (1990) 
    ##                                                                           3 
    ##                                         Star Trek: The Wrath of Khan (1982) 
    ##                                                                           4 
    ##                                  Star Trek III: The Search for Spock (1984) 
    ##                                                                           4 
    ##                                        Star Trek IV: The Voyage Home (1986) 
    ##                                                                           4 
    ##                                                       Batman Returns (1992) 
    ##                                                                           3 
    ##                                                           Young Guns (1988) 
    ##                                                                           3 
    ##                                                          Under Siege (1992) 
    ##                                                                           3 
    ##                                                      Raising Arizona (1987) 
    ##                                                                           4 
    ##                                                             Sneakers (1992) 
    ##                                                                           3 
    ##                                            Last of the Mohicans, The (1992) 
    ##                                                                           5 
    ##                                                 George of the Jungle (1997) 
    ##                                                                           5 
    ##                                            Hunt for Red October, The (1990) 
    ##                                                                           4 
    ##                                                      Full Monty, The (1997) 
    ##                                                                           5 
    ##                                                Sense and Sensibility (1995) 
    ##                                                                           5 
    ##                                                         Bed of Roses (1996) 
    ##                                                                           5 
    ##                                                      River Wild, The (1994) 
    ##                                                                           3 
    ##                                                 English Patient, The (1996) 
    ##                                                                           3 
    ##                                                     Fierce Creatures (1997) 
    ##                                                                           5 
    ##                                            In the Name of the Father (1993) 
    ##                                                                           4 
    ##                                                     Schindler's List (1993) 
    ##                                                                           5 
    ##                                                    Conspiracy Theory (1997) 
    ##                                                                           4 
    ##                                      One Flew Over the Cuckoo's Nest (1975) 
    ##                                                                           4 
    ##                                                      Dangerous Minds (1995) 
    ##                                                                           4 
    ##                                                             Clueless (1995) 
    ##                                                                           3 
    ##                                               Miracle on 34th Street (1994) 
    ##                                                                           5 
    ##                                               Star Trek: Generations (1994) 
    ##                                                                           4 
    ##                    Adventures of Priscilla, Queen of the Desert, The (1994) 
    ##                                                                           4 
    ##                                                            True Lies (1994) 
    ##                                                                           3 
    ##                                                 Addams Family Values (1993) 
    ##                                                                           3 
    ##                                                         Black Beauty (1994) 
    ##                                                                           3 
    ##                                                  Fear of a Black Hat (1993) 
    ##                                                                           3 
    ##                                              Man Without a Face, The (1993) 
    ##                                                                           4 
    ##                                                       Mrs. Doubtfire (1993) 
    ##                                                                           4 
    ##                                                Three Musketeers, The (1993) 
    ##                                                                           3 
    ##                                               Brady Bunch Movie, The (1995) 
    ##                                                                           4 
    ##                                                                Ghost (1990) 
    ##                                                                           4 
    ##                                                               Batman (1989) 
    ##                                                                           3 
    ##                                                            Pinocchio (1940) 
    ##                                                                           4 
    ##                                                                 Jack (1996) 
    ##                                                                           2 
    ##                                                     My Favorite Year (1982) 
    ##                                                                           3 
    ##                                                     Parent Trap, The (1961) 
    ##                                                                           3 
    ##                                                           Cinderella (1950) 
    ##                                                                           5 
    ##                                                  Alice in Wonderland (1951) 
    ##                                                                           4 
    ##                               William Shakespeare's Romeo and Juliet (1996) 
    ##                                                                           4 
    ##                                           E.T. the Extra-Terrestrial (1982) 
    ##                                                                           2 
    ##                                                To Kill a Mockingbird (1962) 
    ##                                                                           4 
    ##                                                           Highlander (1986) 
    ##                                                                           5 
    ##                                                             Fantasia (1940) 
    ##                                                                           5 
    ##                                                             Heathers (1989) 
    ##                                                                           3 
    ##                                   Butch Cassidy and the Sundance Kid (1969) 
    ##                                                                           4 
    ##                                                               Grease (1978) 
    ##                                                                           5 
    ##                                             Cry, the Beloved Country (1995) 
    ##                                                                           4 
    ##                                                     Jungle Book, The (1994) 
    ##                                                                           4 
    ##                                                   Courage Under Fire (1996) 
    ##                                                                           3 
    ##                                                          Dragonheart (1996) 
    ##                                                                           3 
    ## Dr. Strangelove or: How I Learned to Stop Worrying and Love the Bomb (1963) 
    ##                                                                           3 
    ##                                                   North by Northwest (1959) 
    ##                                                                           3 
    ##                                                         My Fair Lady (1964) 
    ##                                                                           4 
    ##                                                        Roman Holiday (1953) 
    ##                                                                           4 
    ##                                                         Sunset Blvd. (1950) 
    ##                                                                           3 
    ##                                                      His Girl Friday (1940) 
    ##                                                                           4 
    ##                                                It's a Wonderful Life (1946) 
    ##                                                                           5 
    ##                                                     Bringing Up Baby (1938) 
    ##                                                                           3 
    ##                                                Cat on a Hot Tin Roof (1958) 
    ##                                                                           4 
    ##                                                Rebel Without a Cause (1955) 
    ##                                                                           4 
    ##                                               Magnificent Seven, The (1954) 
    ##                                                                           3 
    ##                                                           Annie Hall (1977) 
    ##                                                                           4 
    ##                                                    Great Escape, The (1963) 
    ##                                                                           4 
    ##                                                               Gandhi (1982) 
    ##                                                                           4 
    ##                                                  Killing Fields, The (1984) 
    ##                                                                           3 
    ##                                 My Life as a Dog (Mitt liv som hund) (1985) 
    ##                                                                           4 
    ##                                           Die Hard: With a Vengeance (1995) 
    ##                                                                           3 
    ##                                                Walk in the Clouds, A (1995) 
    ##                                                                           4 
    ##                                           Interview with the Vampire (1994) 
    ##                                                                           3 
    ##                                             Clear and Present Danger (1994) 
    ##                                                                           4 
    ##                                                                Speed (1994) 
    ##                                                                           4 
    ##                                                     Another Stakeout (1993) 
    ##                                                                           1 
    ##                                                            Coneheads (1993) 
    ##                                                                           1 
    ##                                                 Beauty and the Beast (1991) 
    ##                                                                           3 
    ##                                         Hunchback of Notre Dame, The (1996) 
    ##                                                                           3 
    ##                                                     Big Squeeze, The (1996) 
    ##                                                                           3 
    ##                                                          Rear Window (1954) 
    ##                                                                           4 
    ##                                              Sword in the Stone, The (1963) 
    ##                                                                           4 
    ##                                        Robin Hood: Prince of Thieves (1991) 
    ##                                                                           4 
    ##                                                      Sophie's Choice (1982) 
    ##                                                                           3 
    ##                                                   Christmas Carol, A (1938) 
    ##                                                                           4 
    ##                                                     Paris Is Burning (1990) 
    ##                                                                           5 
    ##                                                       Quiet Man, The (1952) 
    ##                                                                           4 
    ##                                          Once Upon a Time in America (1984) 
    ##                                                                           4 
    ##                                                                Glory (1989) 
    ##                                                                           4 
    ##                                                          Stand by Me (1986) 
    ##                                                                           5 
    ##                                                   Pump Up the Volume (1990) 
    ##                                                                           4 
    ##                                                 Fried Green Tomatoes (1991) 
    ##                                                                           4 
    ##                                                  Conan the Barbarian (1981) 
    ##                                                                           2 
    ##                                                  In the Line of Fire (1993) 
    ##                                                                           4 
    ##                                                           Persuasion (1995) 
    ##                                                                           5 
    ##                                                         Little Women (1994) 
    ##                                                                           4 
    ##                                            House of the Spirits, The (1993) 
    ##                                                                           2 
    ##                                                  Singin' in the Rain (1952) 
    ##                                                                           4 
    ##                                                      Enchanted April (1991) 
    ##                                                                           4 
    ##                                                    Strictly Ballroom (1992) 
    ##                                                                           5 
    ##                                                           To Die For (1995) 
    ##                                                                           2 
    ##                                                    Circle of Friends (1995) 
    ##                                                                           5 
    ##                                                     Immortal Beloved (1994) 
    ##                                                                           4 
    ##                                                                 Nell (1994) 
    ##                                                                           4 
    ##                                                                 Dave (1993) 
    ##                                                                           3 
    ##                                                      Made in America (1993) 
    ##                                                                           3 
    ##                                                         Philadelphia (1993) 
    ##                                                                           5 
    ##                                                         Pretty Woman (1990) 
    ##                                                                           3 
    ##                                                          Real Genius (1985) 
    ##                                                                           4 
    ##                                                         Benny & Joon (1993) 
    ##                                                                           4 
    ##                                                           Saint, The (1997) 
    ##                                                                           4 
    ##                                          Father of the Bride Part II (1995) 
    ##                                                                           3 
    ##                                                     Don Juan DeMarco (1995) 
    ##                                                                           5 
    ##                                                               Alaska (1996) 
    ##                                                                           3 
    ##                                                            Pollyanna (1960) 
    ##                                                                           3 
    ##                                                      Shaggy Dog, The (1959) 
    ##                                                                           3 
    ##                                                      Days of Thunder (1990) 
    ##                                                                           3 
    ##                                                       Night on Earth (1991) 
    ##                                                                           3 
    ##                                                          With Honors (1994) 
    ##                                                                           4 
    ##                                               Fox and the Hound, The (1981) 
    ##                                                                           5 
    ##                                        How to Make an American Quilt (1995) 
    ##                                                                           4 
    ##                   Until the End of the World (Bis ans Ende der Welt) (1991) 
    ##                                                                           2 
    ##                                                               Hamlet (1996) 
    ##                                                                           4 
    ##                                                                Balto (1995) 
    ##                                                                           4 
    ##                                                     Oliver & Company (1988) 
    ##                                                                           3 
    ##                                                      Joe's Apartment (1996) 
    ##                                                                           4 
    ##                                                         Bloodsport 2 (1995) 
    ##                                                                           1 
    ##                                                            Tank Girl (1995) 
    ##                                                                           4 
    ##                                   Mrs. Parker and the Vicious Circle (1994) 
    ##                                                                           1 
    ##                                               Some Kind of Wonderful (1987) 
    ##                                                                           5 
    ##                                             Escape to Witch Mountain (1975) 
    ##                                                                           3 
    ##                                                           Doors, The (1991) 
    ##                                                                           3 
    ##                                                            My Family (1995) 
    ##                                                                           4 
    ##                                                        Young Guns II (1990) 
    ##                                                                           3 
    ##                                        Under Siege 2: Dark Territory (1995) 
    ##                                                                           2 
    ##                                        Ready to Wear (Pret-A-Porter) (1994) 
    ##                                                                           3 
    ##                                                       That Darn Cat! (1965) 
    ##                                                                           3 
    ##                                                   Jefferson in Paris (1995) 
    ##                                                                           2

``` r
######part three with our own daata

movie<-read.csv("C:/Users/Administrator/Desktop/machine learning/MOVIE_DATA1.csv")
index<-unique(movie$Reg.Ids)

ratingmovie<-acast(movie,Reg.Ids~Movie.Name)
ratingmovie_real<-as(ratingmovie,"realRatingMatrix")

as(ratingmovie_real,"list")
```

    ## $`1`
    ##  Captain America       Cinderella         Deadpool        Furious 7 
    ##                3                4                4                4 
    ##   Justice League    Suicide Squad      The Lobster The Longest Ride 
    ##                3                3                3                4 
    ##     The Revenant   Thor: Ragnarok 
    ##                4                4 
    ## 
    ## $`2`
    ##        Ae Dil Hai Mushkil        Annabelle:Creation 
    ##                         4                         1 
    ##                   Arrival           Bajirao Mastani 
    ##                         2                         3 
    ##      Beauty and the Beast           Bridge of Spies 
    ##                         3                         3 
    ##                  Deadpool              Dear Zindagi 
    ##                         3                         4 
    ##                   Dunkirk             Hacksaw Ridge 
    ##                         5                         5 
    ##                 Insurgent                Passengers 
    ##                         3                         2 
    ## The Man Who Knew Infinity              Wonder Women 
    ##                         5                         3 
    ## 
    ## $`3`
    ##                        Bahubali                    Daddy's Home 
    ##                               4                               4 
    ##                              IT                   Jurasic world 
    ##                               2                               4 
    ##                   Kung Fu Panda Mission Impossible:Rogue Nation 
    ##                               4                               3 
    ##                           Moana                   Suicide Squad 
    ##                               5                               3 
    ##                 The Jungle Book                    Wonder Woman 
    ##                               5                               4 
    ## 
    ## $`6`
    ##                       Captain America: Civil War 
    ##                                                4 
    ##                                         Deadpool 
    ##                                                4 
    ##                                   Doctor Strange 
    ##                                                4 
    ##          Fantastic Beasts and Where to Find Them 
    ##                                                4 
    ##                                    Me Before You 
    ##                                                4 
    ##                                            Moana 
    ##                                                4 
    ## Pirates of the Caribbean: Dead Men Tell No Tales 
    ##                                                3 
    ##                                Star Trek: Beyond 
    ##                                                4 
    ##                                    Suicide Squad 
    ##                                                3 
    ##                                       The Intern 
    ##                                                4 
    ##                                     Wonder Woman 
    ##                                                4 
    ## 
    ## $`7`
    ##                            Ant-Man                   Assassin's Creed 
    ##                                  4                                  3 
    ##            Avengers: Age of Ultron                           Bad Moms 
    ##                                  4                                  3 
    ## Batman v Superman: Dawn of Justice         Captain America: Civil War 
    ##                                  3                                  4 
    ##                              Carol                             Cars 3 
    ##                                  4                                  3 
    ##                              Creed                           Deadpool 
    ##                                  4                                  4 
    ##                            Everest                     Fantastic Four 
    ##                                  4                                  2 
    ##               Fifty Shades of Grey                              Focus 
    ##                                  2                                  3 
    ##                          Furious 7                         Inside Out 
    ##                                  4                                  4 
    ##                             It (I)                  Jupiter Ascending 
    ##                                  4                                  3 
    ##                     Jurassic World        Kingsman: The Golden Circle 
    ##                                  4                                  4 
    ##                     La La Land (I)                 Mad Max: Fury Road 
    ##                                  4                                  4 
    ## Mission: Impossible - Rogue Nation                    Pitch Perfect 2 
    ##                                  4                                  3 
    ##                               Room                        San Andreas 
    ##                                  4                                  3 
    ##                               Sing             Spider-Man: Homecoming 
    ##                                  4                                  4 
    ##                          Spotlight                      Suicide Squad 
    ##                                  4                                  3 
    ##                              Ted 2                      The Big Short 
    ##                                  3                                  4 
    ##                    The Danish Girl                   The Longest Ride 
    ##                                  4                                  4 
    ##                        The Martian                       The Revenant 
    ##                                  4                                  4 
    ##                     Thor: Ragnarok      Transformers: The Last Knight 
    ##                                  4                                  3 
    ##                  X-Men: Apocalypse 
    ##                                  4 
    ## 
    ## $`9`
    ##     Gerald's Game                IT    Justice league             Logan 
    ##                 3                 4                 3                 4 
    ## London has fallen Now you see me: 2             Sully The hateful Eight 
    ##                 3                 3                 4                 4 
    ##    Thor: Ragnarok      Wonder Women 
    ##                 5                 4 
    ## 
    ## $`13`
    ##     Avengers: Age of Ultron        Fifty Shades of Grey 
    ##                           4                           3 
    ##              Justice League Kingsman: The Golden Circle 
    ##                           1                           4 
    ##          Mad Max: Fury Road             Pitch Perfect 2 
    ##                           4                           2 
    ##                     Spectre      Spider-Man: Homecoming 
    ##                           4                           3 
    ##                The Revenant              Thor: Ragnarok 
    ##                           4                           3 
    ##                Wonder Woman 
    ##                           3 
    ## 
    ## $`14`
    ##            Avengers: Age of Ultron                        Baby Driver 
    ##                                  4                                  4 
    ## Batman v Superman: Dawn of Justice         Captain America: Civil War 
    ##                                  3                                  4 
    ##                     Doctor Strange               Fifty Shades of Grey 
    ##                                  3                                  1 
    ##                           Southpaw                      Suicide Squad 
    ##                                  4                                  4 
    ##                     Thor: Ragnarok                  X-Men: Apocalypse 
    ##                                  5                                  3 
    ## 
    ## $`20`
    ##                        Ant-Man        Avengers: Age of Ultron 
    ##                              4                              3 
    ##                 Fantastic Four                      Furious 7 
    ##                              5                              3 
    ## Guardians of the Galaxy Vol. 2                             It 
    ##                              4                              3 
    ##                 Justice League             Mad Max: Fury Road 
    ##                              4                              4 
    ##                The Jungle Book 
    ##                              5 
    ## 
    ## $`28`
    ##             Avengers:age of ultron Batman vs superman:dawn of justice 
    ##                                  5                                  3 
    ##          Captain america:civil war                     Doctor strange 
    ##                                  5                                  3 
    ##                   Fast & furious 7               Fifty shades of grey 
    ##                                  5                                  4 
    ##                        Jungle book                      Jurasic world 
    ##                                  3                                  3 
    ##                  Mad max fury road                    Xmen:apocalypse 
    ##                                  4                                  4 
    ## 
    ## $`29`
    ##         Black Mass       Crimson Peak       Experimenter 
    ##                  4                  5                  4 
    ##                Joy Mad Max: Fury Road        Point Break 
    ##                  4                  3                  1 
    ##           Southpaw            Spectre              Ted 2 
    ##                  3                  1                  4 
    ##      The Big Short 
    ##                  5 
    ## 
    ## $`32`
    ##      Cindrella Doctor strange      Furious 7             It Jurassic world 
    ##              4              3              4              4              4 
    ##  Kunfu panda 2     La la land          Ted 2   The Revenant       Zootopia 
    ##              4              4              4              4              3 
    ## 
    ## $`34`
    ##          Baby Driver       Daddy's Home 2       Doctor Strange 
    ##                    4                    3                    4 
    ## Fifty Shades of Grey                   It           La la Land 
    ##                    2                    4                    4 
    ##        Me before you                Moana        Suicide Squad 
    ##                    4                    4                    3 
    ##          The Lobster          The Martian         The Revenant 
    ##                    3                    4                    4 
    ##               Trolls 
    ##                    4 
    ## 
    ## $`36`
    ## A Bad Moms Christmas             Bahubali         Daddy's Home 
    ##                    4                    5                    4 
    ##        Don't Breathe                   IT       Justice League 
    ##                    5                    1                    5 
    ##        Kung Fu Panda       Thor: Ragnarok                Tiger 
    ##                    5                    5                    4 
    ## 
    ## $`37`
    ##                         Ant-Man      Captain America: Civil War 
    ##                               2                               5 
    ##                         Chappie                      Cinderella 
    ##                               3                               4 
    ##                       Furious 7            Hotel Transylvania 2 
    ##                               4                               4 
    ##                      Inside Out                  Jurassic World 
    ##                               4                               4 
    ##                   Me Before You Mission Impossible:Rogue Nation 
    ##                               4                               5 
    ##                     San Andreas                        The Duff 
    ##                               4                               3 
    ##                     The Martian               X-Men: Apocalypse 
    ##                               4                               4 
    ## 
    ## $`38`
    ##                            Airlift                Annabelle: Creation 
    ##                                  4                                  4 
    ##                  Bareilly Ki Barfi                     Cinderella (I) 
    ##                                  4                                  5 
    ##                             Dangal                       Dear Zindagi 
    ##                                  5                                  3 
    ##               Fifty Shades of Grey                     Jurassic World 
    ##                                  3                                  4 
    ##                      Kapoor & Sons Mission: Impossible - Rogue Nation 
    ##                                  4                                  5 
    ##                   Secret Superstar       Star Wars: The Force Awakens 
    ##                                  5                                  4 
    ##                              Ted 2                    The Conjuring 2 
    ##                                  4                                  4 
    ##                    The Jungle Book                       Wonder Woman 
    ##                                  4                                  4 
    ## 
    ## $`40`
    ##                 Avengers: Age of Ultron 
    ##                                       4 
    ##                                   Burnt 
    ##                                       3 
    ##                                 Chappie 
    ##                                       3 
    ##                              Cinderella 
    ##                                       3 
    ##                                Deadpool 
    ##                                       4 
    ##                          Doctor Strange 
    ##                                       4 
    ##                                 Everest 
    ##                                       5 
    ## Fantastic Beasts and Where to Find Them 
    ##                                       4 
    ##          Guardians of the Galaxy Vol. 2 
    ##                                       5 
    ##                    Hotel Transylvania 2 
    ##                                       5 
    ##                              Inside Out 
    ##                                       4 
    ##                    Insidious: Chapter 3 
    ##                                       4 
    ##                       Jupiter Ascending 
    ##                                       3 
    ##                          Jurassic World 
    ##                                       4 
    ##                           Me Before You 
    ##                                       4 
    ##                                 Minions 
    ##                                       4 
    ##      Mission: Impossible - Rogue Nation 
    ##                                       4 
    ##                             Paper Towns 
    ##                                       4 
    ##                  Spider-Man: Homecoming 
    ##                                       3 
    ##                                   Ted 2 
    ##                                       4 
    ##                      The Age of Adaline 
    ##                                       4 
    ##                         The Conjuring 2 
    ##                                       4 
    ##                         The Jungle Book 
    ##                                       5 
    ##                            The Revenant 
    ##                                       5 
    ##                              Trainwreck 
    ##                                       4 
    ##                                  Trolls 
    ##                                       4 
    ##                            Wonder Woman 
    ##                                       4 
    ## 
    ## $`41`
    ##    Arrival      Focus   Geostorm       Gold         IT       Lion 
    ##          3          4          5          4          2          4 
    ##  Moonlight       Room The Square     Trolls 
    ##          3          5          2          1 
    ## 
    ## $`45`
    ## Batman v Superman: Dawn of Justice                           Deadpool 
    ##                                  3                                  3 
    ##                                 It                     Jurassic World 
    ##                                  3                                  4 
    ##                     Justice League Mission: Impossible - Rogue Nation 
    ##                                  3                                  3 
    ##                        San Andreas             Spider-Man: Homecoming 
    ##                                  3                                  3 
    ##       Star Wars: The Force Awakens                      Suicide Squad 
    ##                                  3                                  3 
    ## 
    ## $`46`
    ## Avengers : Age of Ultron                 Deadpool     Fifty shades of Grey 
    ##                        4                        4                        4 
    ##                  Inferno             Jason Bourne                    Logan 
    ##                        4                        4                        4 
    ##             Spotlight(I)          The Danish Girl              The Martian 
    ##                        5                        3                        5 
    ##             The Revenant 
    ##                        5 
    ## 
    ## $`48`
    ##                                 Ant-Man 
    ##                                       3 
    ##              Captain America: Civil War 
    ##                                       4 
    ##                                   Creed 
    ##                                       4 
    ##                                Deadpool 
    ##                                       4 
    ## Fantastic Beasts and Where to Find Them 
    ##                                       5 
    ##                              Inside Out 
    ##                                       4 
    ##                               Rogue One 
    ##                                       2 
    ##                                    Room 
    ##                                       4 
    ##                                   Split 
    ##                                       3 
    ##                             The Martian 
    ##                                       4 
    ## 
    ## $`50`
    ## 13 Hours: The Secret Soldiers of Benghazi 
    ##                                         4 
    ##                                Concussion 
    ##                                         3 
    ##                                     Keanu 
    ##                                         3 
    ##                       Our Kind of Traitor 
    ##                                         4 
    ##                                      Room 
    ##                                         4 
    ##                               San Andreas 
    ##                                         3 
    ##                                   The BFG 
    ##                                         3 
    ##                           The Conjuring 2 
    ##                                         4 
    ##                The Huntsman: Winter's War 
    ##                                         3 
    ##                                 The Visit 
    ##                                         3 
    ## 
    ## $`52`
    ##                 Ant-Man Avengers: Age of Ultron               Furious 7 
    ##                       3                       3                       4 
    ##          Jurassic World      Mad Max: Fury Road                   Moana 
    ##                       3                       4                       3 
    ##              Passengers          The Accountant             The Martian 
    ##                       4                       4                       4 
    ##            Wonder Woman 
    ##                       4 
    ## 
    ## $`53`
    ##   A Bad Moms Christmas Avengers:Age of Ultron      Blade Runner 2049 
    ##                      3                      4                      4 
    ##   Fifty Shades of Grey              Furious 7                 Jigsaw 
    ##                      5                      5                      4 
    ##      Mad Max:Fury Road                Spectre   Spiderman:Homecoming 
    ##                      4                      4                      3 
    ##           The Revenant 
    ##                      5 
    ## 
    ## $`58`
    ##       Baahubali        Baywatch          Dangal        Deadpool 
    ##               4               1               4               5 
    ##   Don't Breathe              IT Kung Fu Panda 3          Newton 
    ##               4               5               4               5 
    ##   Sausage Party        Zootopia 
    ##               3               4 
    ## 
    ## $`61`
    ##      A Father's Journey                   Ameen      Bastards y Diablos 
    ##                       3                       4                       4 
    ##           Fuera de foco I Am My Sister's Keeper         Mithila Makhaan 
    ##                       5                       4                       5 
    ##                   Partu  Something Happens Here                 Taandro 
    ##                       5                       5                       5 
    ##    Take 2: The Audition 
    ##                       4 
    ## 
    ## $`64`
    ##                 Avengers: Age of Ultron 
    ##                                       4 
    ##      Batman v Superman: Dawn of Justice 
    ##                                       3 
    ##              Captain America: Civil War 
    ##                                       4 
    ## Fantastic Beasts and Where to Find Them 
    ##                                       5 
    ##                          Fantastic Four 
    ##                                       4 
    ##                               Furious 7 
    ##                                       3 
    ##                          Jurassic World 
    ##                                       5 
    ##                          Justice League 
    ##                                       3 
    ##                      Mad Max: Fury Road 
    ##                                       3 
    ## 
    ## $`69`
    ##                            Ant - Man                        Atomic Blonde 
    ##                                    4                                    4 
    ## Batman Vs Superman - Dawn of justice          Captain America - Civil war 
    ##                                    4                                    4 
    ##                         Daddy's home                 Fifty shades of grey 
    ##                                    3                                    3 
    ##         Kingsman - The golden circle               Spiderman - Homecoming 
    ##                                    4                                    3 
    ##                        Suicide Squad                          Wonderwomen 
    ##                                    3                                    4 
    ## 
    ## $`70`
    ##                 Ant-Man Avengers: Age of Ultron                   Creed 
    ##                       3                       3                       4 
    ##                Deadpool              Inside Out      Mad Max: Fury Road 
    ##                       3                       4                       3 
    ##                    Room                 Sicario               Spotlight 
    ##                       4                       5                       4 
    ##           Suicide Squad The Man from U.N.C.L.E.             The Martian 
    ##                       3                       4                       4 
    ## 
    ## $`71`
    ##                                         Deadpool 
    ##                                                4 
    ##                             Fifty Shades of Grey 
    ##                                                5 
    ##                                        Furious 7 
    ##                                                4 
    ##                                   Jurassic World 
    ##                                                4 
    ## Pirates of the caribbeab: Dead Men Tell No Tales 
    ##                                                3 
    ##                                      San Andreas 
    ##                                                4 
    ##                                          Spectre 
    ##                                                4 
    ##                                    Suicide Squad 
    ##                                                4 
    ##                                            Ted 2 
    ##                                                5 
    ##                                   The Accountant 
    ##                                                4 
    ##                            The Magnificent seven 
    ##                                                3 
    ##                                      The Martian 
    ##                                                4 
    ## 
    ## $`72`
    ## Hidden Figures          Keanu           Lion           room    San Andreas 
    ##              3              3              4              4              3 
    ##        The BFG 
    ##              3 
    ## 
    ## $`74`
    ##                       Ant-Man       Avengers: Age of Ultron 
    ##                             3                             4 
    ##                        Dangal                      Deadpool 
    ##                             4                             5 
    ##                            It            Mad Max: Fury Road 
    ##                             5                             4 
    ##                       Spectre       Spider-Man: Homecomming 
    ##                             4                             4 
    ##                         Ted 2       The Man from U.N.C.L.E. 
    ##                             3                             4 
    ##                   The Martian                  The Revenant 
    ##                             4                             4 
    ## Transformers: The Last Knight                  Wonder Woman 
    ##                             3                             5 
    ## 
    ## $`79`
    ##       Cinderella            Don 2 Fast & Furious 5        Jolly LLB 
    ##                3                5                5                5 
    ##        King Kong     Kungfu Panda       Real Steel Secret Superstar 
    ##                4                5                4                3 
    ##       Terminator     Transformers 
    ##                5                5 
    ## 
    ## $`80`
    ##                                 Ant-Man 
    ##                                       4 
    ##                 Avengers: Age of Ultron 
    ##                                       3 
    ##                       Batman v Superman 
    ##                                       4 
    ## Fantastic Beasts and Where to Find Them 
    ##                                       4 
    ##                                      It 
    ##                                       2 
    ##                          Jurassic World 
    ##                                       4 
    ##                          Justice League 
    ##                                       3 
    ##                         The Conjuring 2 
    ##                                       4 
    ##                               The Mummy 
    ##                                       2 
    ##                            Wonder Woman 
    ##                                       5 
    ## 
    ## $`83`
    ##         Baywatch           Dangal         Deadpool    Dirty Grandpa 
    ##                4                4                3                3 
    ##        Furious 7   Jurassic World  Kung Fu Panda 3 Now You See Me 2 
    ##                3                3                3                4 
    ##      San Andreas  The Conjuring 2  The Jungle Book 
    ##                4                5                4 
    ## 
    ## $`84`
    ##      Avengers: Age of Ultron (2015) Batman v Superman : Dawn of Justice 
    ##                                   4                                   4 
    ##          Captain America: Civil War                            Deadpool 
    ##                                   4                                   4 
    ##               Jurassic World (2015)                      Justice League 
    ##                                   4                                   4 
    ##           Terminator Genisys (2015)                      Thor: Ragnarok 
    ##                                   3                                   4 
    ##                        Wonder Woman 
    ##                                   4 
    ## 
    ## $`85`
    ##       Befikre        Dangal Dirty Grandpa  Ghazi Attack    La La Land 
    ##             2             5             4             4             4 
    ##        Newton        Toilet       Trapped     Tubelight  Wonder Woman 
    ##             4             4             3             3             4 
    ## 
    ## $`89`
    ##              Bedeviled    Birth of the Dragon Boo! A Madea Halloween 
    ##                      4                      5                      5 
    ##               Cell (I)   Fifty Shades of Grey            Knock Knock 
    ##                      4                      4                      5 
    ##            Rupture (I)                Shut In     Wolves at the Door 
    ##                      5                      5                      5 
    ##            Zoolander 2 
    ##                      5

``` r
test_movie<-ratingmovie_real[c(8),]
train_movie<-ratingmovie_real[-c(8),]

recom<-Recommender(train_movie,method="UBCF")
recomp<-Recommender(train_movie,method="POPULAR")
recoms<-Recommender(train_movie,method="SVD")

#predict 
mypredict<-predict(recom,test_movie,n=3)
mypredictP<-predict(recomp,test_movie,n=3)
mypredicts<-predict(recoms,test_movie,n=3)



as(mypredict,"list")
```

    ## $`14`
    ## [1] "Fantastic Beasts and Where to Find Them"
    ## [2] "The Jungle Book"                        
    ## [3] "Wonder Woman"

``` r
as(mypredictP,"list")
```

    ## $`14`
    ## [1] "The Revenant"    "The Jungle Book" "Wonder Woman"

``` r
as(mypredicts,"list")
```

    ## $`14`
    ## [1] "Secret Superstar" "Trolls"           "It"

``` r
as(test_movie,"list")
```

    ## $`14`
    ##            Avengers: Age of Ultron                        Baby Driver 
    ##                                  4                                  4 
    ## Batman v Superman: Dawn of Justice         Captain America: Civil War 
    ##                                  3                                  4 
    ##                     Doctor Strange               Fifty Shades of Grey 
    ##                                  3                                  1 
    ##                           Southpaw                      Suicide Squad 
    ##                                  4                                  4 
    ##                     Thor: Ragnarok                  X-Men: Apocalypse 
    ##                                  5                                  3

``` r
as(train_movie,"list")
```

    ## $`1`
    ##  Captain America       Cinderella         Deadpool        Furious 7 
    ##                3                4                4                4 
    ##   Justice League    Suicide Squad      The Lobster The Longest Ride 
    ##                3                3                3                4 
    ##     The Revenant   Thor: Ragnarok 
    ##                4                4 
    ## 
    ## $`2`
    ##        Ae Dil Hai Mushkil        Annabelle:Creation 
    ##                         4                         1 
    ##                   Arrival           Bajirao Mastani 
    ##                         2                         3 
    ##      Beauty and the Beast           Bridge of Spies 
    ##                         3                         3 
    ##                  Deadpool              Dear Zindagi 
    ##                         3                         4 
    ##                   Dunkirk             Hacksaw Ridge 
    ##                         5                         5 
    ##                 Insurgent                Passengers 
    ##                         3                         2 
    ## The Man Who Knew Infinity              Wonder Women 
    ##                         5                         3 
    ## 
    ## $`3`
    ##                        Bahubali                    Daddy's Home 
    ##                               4                               4 
    ##                              IT                   Jurasic world 
    ##                               2                               4 
    ##                   Kung Fu Panda Mission Impossible:Rogue Nation 
    ##                               4                               3 
    ##                           Moana                   Suicide Squad 
    ##                               5                               3 
    ##                 The Jungle Book                    Wonder Woman 
    ##                               5                               4 
    ## 
    ## $`6`
    ##                       Captain America: Civil War 
    ##                                                4 
    ##                                         Deadpool 
    ##                                                4 
    ##                                   Doctor Strange 
    ##                                                4 
    ##          Fantastic Beasts and Where to Find Them 
    ##                                                4 
    ##                                    Me Before You 
    ##                                                4 
    ##                                            Moana 
    ##                                                4 
    ## Pirates of the Caribbean: Dead Men Tell No Tales 
    ##                                                3 
    ##                                Star Trek: Beyond 
    ##                                                4 
    ##                                    Suicide Squad 
    ##                                                3 
    ##                                       The Intern 
    ##                                                4 
    ##                                     Wonder Woman 
    ##                                                4 
    ## 
    ## $`7`
    ##                            Ant-Man                   Assassin's Creed 
    ##                                  4                                  3 
    ##            Avengers: Age of Ultron                           Bad Moms 
    ##                                  4                                  3 
    ## Batman v Superman: Dawn of Justice         Captain America: Civil War 
    ##                                  3                                  4 
    ##                              Carol                             Cars 3 
    ##                                  4                                  3 
    ##                              Creed                           Deadpool 
    ##                                  4                                  4 
    ##                            Everest                     Fantastic Four 
    ##                                  4                                  2 
    ##               Fifty Shades of Grey                              Focus 
    ##                                  2                                  3 
    ##                          Furious 7                         Inside Out 
    ##                                  4                                  4 
    ##                             It (I)                  Jupiter Ascending 
    ##                                  4                                  3 
    ##                     Jurassic World        Kingsman: The Golden Circle 
    ##                                  4                                  4 
    ##                     La La Land (I)                 Mad Max: Fury Road 
    ##                                  4                                  4 
    ## Mission: Impossible - Rogue Nation                    Pitch Perfect 2 
    ##                                  4                                  3 
    ##                               Room                        San Andreas 
    ##                                  4                                  3 
    ##                               Sing             Spider-Man: Homecoming 
    ##                                  4                                  4 
    ##                          Spotlight                      Suicide Squad 
    ##                                  4                                  3 
    ##                              Ted 2                      The Big Short 
    ##                                  3                                  4 
    ##                    The Danish Girl                   The Longest Ride 
    ##                                  4                                  4 
    ##                        The Martian                       The Revenant 
    ##                                  4                                  4 
    ##                     Thor: Ragnarok      Transformers: The Last Knight 
    ##                                  4                                  3 
    ##                  X-Men: Apocalypse 
    ##                                  4 
    ## 
    ## $`9`
    ##     Gerald's Game                IT    Justice league             Logan 
    ##                 3                 4                 3                 4 
    ## London has fallen Now you see me: 2             Sully The hateful Eight 
    ##                 3                 3                 4                 4 
    ##    Thor: Ragnarok      Wonder Women 
    ##                 5                 4 
    ## 
    ## $`13`
    ##     Avengers: Age of Ultron        Fifty Shades of Grey 
    ##                           4                           3 
    ##              Justice League Kingsman: The Golden Circle 
    ##                           1                           4 
    ##          Mad Max: Fury Road             Pitch Perfect 2 
    ##                           4                           2 
    ##                     Spectre      Spider-Man: Homecoming 
    ##                           4                           3 
    ##                The Revenant              Thor: Ragnarok 
    ##                           4                           3 
    ##                Wonder Woman 
    ##                           3 
    ## 
    ## $`20`
    ##                        Ant-Man        Avengers: Age of Ultron 
    ##                              4                              3 
    ##                 Fantastic Four                      Furious 7 
    ##                              5                              3 
    ## Guardians of the Galaxy Vol. 2                             It 
    ##                              4                              3 
    ##                 Justice League             Mad Max: Fury Road 
    ##                              4                              4 
    ##                The Jungle Book 
    ##                              5 
    ## 
    ## $`28`
    ##             Avengers:age of ultron Batman vs superman:dawn of justice 
    ##                                  5                                  3 
    ##          Captain america:civil war                     Doctor strange 
    ##                                  5                                  3 
    ##                   Fast & furious 7               Fifty shades of grey 
    ##                                  5                                  4 
    ##                        Jungle book                      Jurasic world 
    ##                                  3                                  3 
    ##                  Mad max fury road                    Xmen:apocalypse 
    ##                                  4                                  4 
    ## 
    ## $`29`
    ##         Black Mass       Crimson Peak       Experimenter 
    ##                  4                  5                  4 
    ##                Joy Mad Max: Fury Road        Point Break 
    ##                  4                  3                  1 
    ##           Southpaw            Spectre              Ted 2 
    ##                  3                  1                  4 
    ##      The Big Short 
    ##                  5 
    ## 
    ## $`32`
    ##      Cindrella Doctor strange      Furious 7             It Jurassic world 
    ##              4              3              4              4              4 
    ##  Kunfu panda 2     La la land          Ted 2   The Revenant       Zootopia 
    ##              4              4              4              4              3 
    ## 
    ## $`34`
    ##          Baby Driver       Daddy's Home 2       Doctor Strange 
    ##                    4                    3                    4 
    ## Fifty Shades of Grey                   It           La la Land 
    ##                    2                    4                    4 
    ##        Me before you                Moana        Suicide Squad 
    ##                    4                    4                    3 
    ##          The Lobster          The Martian         The Revenant 
    ##                    3                    4                    4 
    ##               Trolls 
    ##                    4 
    ## 
    ## $`36`
    ## A Bad Moms Christmas             Bahubali         Daddy's Home 
    ##                    4                    5                    4 
    ##        Don't Breathe                   IT       Justice League 
    ##                    5                    1                    5 
    ##        Kung Fu Panda       Thor: Ragnarok                Tiger 
    ##                    5                    5                    4 
    ## 
    ## $`37`
    ##                         Ant-Man      Captain America: Civil War 
    ##                               2                               5 
    ##                         Chappie                      Cinderella 
    ##                               3                               4 
    ##                       Furious 7            Hotel Transylvania 2 
    ##                               4                               4 
    ##                      Inside Out                  Jurassic World 
    ##                               4                               4 
    ##                   Me Before You Mission Impossible:Rogue Nation 
    ##                               4                               5 
    ##                     San Andreas                        The Duff 
    ##                               4                               3 
    ##                     The Martian               X-Men: Apocalypse 
    ##                               4                               4 
    ## 
    ## $`38`
    ##                            Airlift                Annabelle: Creation 
    ##                                  4                                  4 
    ##                  Bareilly Ki Barfi                     Cinderella (I) 
    ##                                  4                                  5 
    ##                             Dangal                       Dear Zindagi 
    ##                                  5                                  3 
    ##               Fifty Shades of Grey                     Jurassic World 
    ##                                  3                                  4 
    ##                      Kapoor & Sons Mission: Impossible - Rogue Nation 
    ##                                  4                                  5 
    ##                   Secret Superstar       Star Wars: The Force Awakens 
    ##                                  5                                  4 
    ##                              Ted 2                    The Conjuring 2 
    ##                                  4                                  4 
    ##                    The Jungle Book                       Wonder Woman 
    ##                                  4                                  4 
    ## 
    ## $`40`
    ##                 Avengers: Age of Ultron 
    ##                                       4 
    ##                                   Burnt 
    ##                                       3 
    ##                                 Chappie 
    ##                                       3 
    ##                              Cinderella 
    ##                                       3 
    ##                                Deadpool 
    ##                                       4 
    ##                          Doctor Strange 
    ##                                       4 
    ##                                 Everest 
    ##                                       5 
    ## Fantastic Beasts and Where to Find Them 
    ##                                       4 
    ##          Guardians of the Galaxy Vol. 2 
    ##                                       5 
    ##                    Hotel Transylvania 2 
    ##                                       5 
    ##                              Inside Out 
    ##                                       4 
    ##                    Insidious: Chapter 3 
    ##                                       4 
    ##                       Jupiter Ascending 
    ##                                       3 
    ##                          Jurassic World 
    ##                                       4 
    ##                           Me Before You 
    ##                                       4 
    ##                                 Minions 
    ##                                       4 
    ##      Mission: Impossible - Rogue Nation 
    ##                                       4 
    ##                             Paper Towns 
    ##                                       4 
    ##                  Spider-Man: Homecoming 
    ##                                       3 
    ##                                   Ted 2 
    ##                                       4 
    ##                      The Age of Adaline 
    ##                                       4 
    ##                         The Conjuring 2 
    ##                                       4 
    ##                         The Jungle Book 
    ##                                       5 
    ##                            The Revenant 
    ##                                       5 
    ##                              Trainwreck 
    ##                                       4 
    ##                                  Trolls 
    ##                                       4 
    ##                            Wonder Woman 
    ##                                       4 
    ## 
    ## $`41`
    ##    Arrival      Focus   Geostorm       Gold         IT       Lion 
    ##          3          4          5          4          2          4 
    ##  Moonlight       Room The Square     Trolls 
    ##          3          5          2          1 
    ## 
    ## $`45`
    ## Batman v Superman: Dawn of Justice                           Deadpool 
    ##                                  3                                  3 
    ##                                 It                     Jurassic World 
    ##                                  3                                  4 
    ##                     Justice League Mission: Impossible - Rogue Nation 
    ##                                  3                                  3 
    ##                        San Andreas             Spider-Man: Homecoming 
    ##                                  3                                  3 
    ##       Star Wars: The Force Awakens                      Suicide Squad 
    ##                                  3                                  3 
    ## 
    ## $`46`
    ## Avengers : Age of Ultron                 Deadpool     Fifty shades of Grey 
    ##                        4                        4                        4 
    ##                  Inferno             Jason Bourne                    Logan 
    ##                        4                        4                        4 
    ##             Spotlight(I)          The Danish Girl              The Martian 
    ##                        5                        3                        5 
    ##             The Revenant 
    ##                        5 
    ## 
    ## $`48`
    ##                                 Ant-Man 
    ##                                       3 
    ##              Captain America: Civil War 
    ##                                       4 
    ##                                   Creed 
    ##                                       4 
    ##                                Deadpool 
    ##                                       4 
    ## Fantastic Beasts and Where to Find Them 
    ##                                       5 
    ##                              Inside Out 
    ##                                       4 
    ##                               Rogue One 
    ##                                       2 
    ##                                    Room 
    ##                                       4 
    ##                                   Split 
    ##                                       3 
    ##                             The Martian 
    ##                                       4 
    ## 
    ## $`50`
    ## 13 Hours: The Secret Soldiers of Benghazi 
    ##                                         4 
    ##                                Concussion 
    ##                                         3 
    ##                                     Keanu 
    ##                                         3 
    ##                       Our Kind of Traitor 
    ##                                         4 
    ##                                      Room 
    ##                                         4 
    ##                               San Andreas 
    ##                                         3 
    ##                                   The BFG 
    ##                                         3 
    ##                           The Conjuring 2 
    ##                                         4 
    ##                The Huntsman: Winter's War 
    ##                                         3 
    ##                                 The Visit 
    ##                                         3 
    ## 
    ## $`52`
    ##                 Ant-Man Avengers: Age of Ultron               Furious 7 
    ##                       3                       3                       4 
    ##          Jurassic World      Mad Max: Fury Road                   Moana 
    ##                       3                       4                       3 
    ##              Passengers          The Accountant             The Martian 
    ##                       4                       4                       4 
    ##            Wonder Woman 
    ##                       4 
    ## 
    ## $`53`
    ##   A Bad Moms Christmas Avengers:Age of Ultron      Blade Runner 2049 
    ##                      3                      4                      4 
    ##   Fifty Shades of Grey              Furious 7                 Jigsaw 
    ##                      5                      5                      4 
    ##      Mad Max:Fury Road                Spectre   Spiderman:Homecoming 
    ##                      4                      4                      3 
    ##           The Revenant 
    ##                      5 
    ## 
    ## $`58`
    ##       Baahubali        Baywatch          Dangal        Deadpool 
    ##               4               1               4               5 
    ##   Don't Breathe              IT Kung Fu Panda 3          Newton 
    ##               4               5               4               5 
    ##   Sausage Party        Zootopia 
    ##               3               4 
    ## 
    ## $`61`
    ##      A Father's Journey                   Ameen      Bastards y Diablos 
    ##                       3                       4                       4 
    ##           Fuera de foco I Am My Sister's Keeper         Mithila Makhaan 
    ##                       5                       4                       5 
    ##                   Partu  Something Happens Here                 Taandro 
    ##                       5                       5                       5 
    ##    Take 2: The Audition 
    ##                       4 
    ## 
    ## $`64`
    ##                 Avengers: Age of Ultron 
    ##                                       4 
    ##      Batman v Superman: Dawn of Justice 
    ##                                       3 
    ##              Captain America: Civil War 
    ##                                       4 
    ## Fantastic Beasts and Where to Find Them 
    ##                                       5 
    ##                          Fantastic Four 
    ##                                       4 
    ##                               Furious 7 
    ##                                       3 
    ##                          Jurassic World 
    ##                                       5 
    ##                          Justice League 
    ##                                       3 
    ##                      Mad Max: Fury Road 
    ##                                       3 
    ## 
    ## $`69`
    ##                            Ant - Man                        Atomic Blonde 
    ##                                    4                                    4 
    ## Batman Vs Superman - Dawn of justice          Captain America - Civil war 
    ##                                    4                                    4 
    ##                         Daddy's home                 Fifty shades of grey 
    ##                                    3                                    3 
    ##         Kingsman - The golden circle               Spiderman - Homecoming 
    ##                                    4                                    3 
    ##                        Suicide Squad                          Wonderwomen 
    ##                                    3                                    4 
    ## 
    ## $`70`
    ##                 Ant-Man Avengers: Age of Ultron                   Creed 
    ##                       3                       3                       4 
    ##                Deadpool              Inside Out      Mad Max: Fury Road 
    ##                       3                       4                       3 
    ##                    Room                 Sicario               Spotlight 
    ##                       4                       5                       4 
    ##           Suicide Squad The Man from U.N.C.L.E.             The Martian 
    ##                       3                       4                       4 
    ## 
    ## $`71`
    ##                                         Deadpool 
    ##                                                4 
    ##                             Fifty Shades of Grey 
    ##                                                5 
    ##                                        Furious 7 
    ##                                                4 
    ##                                   Jurassic World 
    ##                                                4 
    ## Pirates of the caribbeab: Dead Men Tell No Tales 
    ##                                                3 
    ##                                      San Andreas 
    ##                                                4 
    ##                                          Spectre 
    ##                                                4 
    ##                                    Suicide Squad 
    ##                                                4 
    ##                                            Ted 2 
    ##                                                5 
    ##                                   The Accountant 
    ##                                                4 
    ##                            The Magnificent seven 
    ##                                                3 
    ##                                      The Martian 
    ##                                                4 
    ## 
    ## $`72`
    ## Hidden Figures          Keanu           Lion           room    San Andreas 
    ##              3              3              4              4              3 
    ##        The BFG 
    ##              3 
    ## 
    ## $`74`
    ##                       Ant-Man       Avengers: Age of Ultron 
    ##                             3                             4 
    ##                        Dangal                      Deadpool 
    ##                             4                             5 
    ##                            It            Mad Max: Fury Road 
    ##                             5                             4 
    ##                       Spectre       Spider-Man: Homecomming 
    ##                             4                             4 
    ##                         Ted 2       The Man from U.N.C.L.E. 
    ##                             3                             4 
    ##                   The Martian                  The Revenant 
    ##                             4                             4 
    ## Transformers: The Last Knight                  Wonder Woman 
    ##                             3                             5 
    ## 
    ## $`79`
    ##       Cinderella            Don 2 Fast & Furious 5        Jolly LLB 
    ##                3                5                5                5 
    ##        King Kong     Kungfu Panda       Real Steel Secret Superstar 
    ##                4                5                4                3 
    ##       Terminator     Transformers 
    ##                5                5 
    ## 
    ## $`80`
    ##                                 Ant-Man 
    ##                                       4 
    ##                 Avengers: Age of Ultron 
    ##                                       3 
    ##                       Batman v Superman 
    ##                                       4 
    ## Fantastic Beasts and Where to Find Them 
    ##                                       4 
    ##                                      It 
    ##                                       2 
    ##                          Jurassic World 
    ##                                       4 
    ##                          Justice League 
    ##                                       3 
    ##                         The Conjuring 2 
    ##                                       4 
    ##                               The Mummy 
    ##                                       2 
    ##                            Wonder Woman 
    ##                                       5 
    ## 
    ## $`83`
    ##         Baywatch           Dangal         Deadpool    Dirty Grandpa 
    ##                4                4                3                3 
    ##        Furious 7   Jurassic World  Kung Fu Panda 3 Now You See Me 2 
    ##                3                3                3                4 
    ##      San Andreas  The Conjuring 2  The Jungle Book 
    ##                4                5                4 
    ## 
    ## $`84`
    ##      Avengers: Age of Ultron (2015) Batman v Superman : Dawn of Justice 
    ##                                   4                                   4 
    ##          Captain America: Civil War                            Deadpool 
    ##                                   4                                   4 
    ##               Jurassic World (2015)                      Justice League 
    ##                                   4                                   4 
    ##           Terminator Genisys (2015)                      Thor: Ragnarok 
    ##                                   3                                   4 
    ##                        Wonder Woman 
    ##                                   4 
    ## 
    ## $`85`
    ##       Befikre        Dangal Dirty Grandpa  Ghazi Attack    La La Land 
    ##             2             5             4             4             4 
    ##        Newton        Toilet       Trapped     Tubelight  Wonder Woman 
    ##             4             4             3             3             4 
    ## 
    ## $`89`
    ##              Bedeviled    Birth of the Dragon Boo! A Madea Halloween 
    ##                      4                      5                      5 
    ##               Cell (I)   Fifty Shades of Grey            Knock Knock 
    ##                      4                      4                      5 
    ##            Rupture (I)                Shut In     Wolves at the Door 
    ##                      5                      5                      5 
    ##            Zoolander 2 
    ##                      5

``` r
#Evalution scheme
scheme<-evaluationScheme(MovieLense,method="split",train=.9,given=3,goodRating=4)
scheme
```

    ## Evaluation scheme with 3 items given
    ## Method: 'split' with 1 run(s).
    ## Training set proportion: 0.900
    ## Good ratings: >=4.000000
    ## Data set: 943 x 1664 rating matrix of class 'realRatingMatrix' with 99392 ratings.

``` r
algo<-list("random items"=list(name="RANDOM"),
           "popular tems"=list(name="POPULAR"),
           "user-based CF"=list(name="UBCF"),
           "svd"=list(name="SVD"))
results<-evaluate(scheme,algo,n=c(1,3,5,10,15,20,25))
```

    ## RANDOM run fold/sample [model time/prediction time]
    ##   1  [0.01sec/1.21sec] 
    ## POPULAR run fold/sample [model time/prediction time]
    ##   1  [0.12sec/0.88sec] 
    ## UBCF run fold/sample [model time/prediction time]
    ##   1  [0.03sec/2.69sec] 
    ## SVD run fold/sample [model time/prediction time]
    ##   1  [0.63sec/0.95sec]

``` r
#True positive rate vs false positive rate
plot(results,annonate=1:4,legend="topleft")
```

![](recom_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
#jester recommendation  on 3 5 and 10
```
