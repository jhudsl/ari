context("Test ari_spin()")

skip_spin <- function(){
  if(Sys.getenv("SKIP_SPIN") != ""){
    skip("Skipping ari_spin()")
  }
}

video <- file.path(tempdir(), "output.mp4")

qmm <- c("I will now perform the Mercutio's speech from Shakespeare's Romeo and Juliet.", 
"O, then, I see Queen Mab hath been with you.
She is the fairies' midwife, and she comes
In shape no bigger than an agate-stone
On the fore-finger of an alderman,
Drawn with a team of little atomies
Athwart men's noses as they lies asleep;
Her wagon-spokes made of long spinners' legs,
The cover of the wings of grasshoppers,
The traces of the smallest spider's web,
The collars of the moonshine's wat'ry beams,
Her whip of cricket's bone; the lash of film;
Her waggoner a small grey-coated gnat,
Not half so big as a round little worm
Pricked from the lazy finger of a maid:
Her chariot is an empty hazelnut
Made by the joiner squirrel or old grub,
Time out o' mind the fairies' coachmakers.
And in this state she gallops night by night
Through lovers' brains, and then they dream of love;
O'er courtiers' knees, that dream on court'sies straight,
O'er lawyers' fingers, who straight dream on fees,
O'er ladies' lips, who straight on kisses dream,
Which oft the angry Mab with blisters plagues,
Because their breaths with sweetmeats tainted are:
Sometime she gallops o'er a courtier's nose,
And then dreams he of smelling out a suit;
And sometime comes she with a tithe-pig's tail
Tickling a parson's nose as a' lies asleep,
Then dreams, he of another benefice:
Sometime she driveth o'er a soldier's neck,
And then dreams he of cutting foreign throats,
Of breaches, ambuscadoes, Spanish blades,
Of healths five-fathom deep; and then anon
Drums in his ear, at which he starts and wakes,
And being thus frighted swears a prayer or two
And sleeps again. This is that very Mab
That plaits the manes of horses in the night,
And bakes the elflocks in foul sluttish hairs,
Which once untangled, much misfortune bodes:
This is the hag, when maids lie on their backs,
That presses them and learns them first to bear,
Making them women of good carriage:
This is she-")

test_that("Ari can process text with over 1500 characters.", {
  skip_on_cran()
  #skip_on_travis()
  skip_spin()
  
  ari_spin(system.file("test", c("mab1.png", "mab2.png"), package = "ari"),
           qmm, video, list_voices()$Id[1])
  
  expect_true(file.size(video) > 50000)
})

unlink(video, force = TRUE)
