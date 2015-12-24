#Final Project: Deadly Shark Attack
#By Muhammad Najib
#Preview of game
#As a deadly Shark, you have to eat all the fish in the pond,
#Move arrow keys to move the shark. If purple fish is eaten, two yellow fishes are born,
#If a yellow fish is eaten, a starfish is born, but eating starfish doesn´t result in a new fish
#Each fish is worth 10 points. Game ends once all fishes have been eaten.

import random, math
from livewires import games, color

#make a screen with the mentioned width and height
games.init(screen_width = 800, screen_height = 600, fps = 50)


class Fish(games.Sprite):
    """ A fish which floats across the screen. """
    #Three different sizes for fish and each has different type/color
    SMALL = 1
    MEDIUM = 2
    LARGE = 3
    images = {SMALL  : games.load_image("starfish.jpg"),
              MEDIUM : games.load_image("Yellowfish.jpg"),
              LARGE  : games.load_image("Purplefish.png") }
    #Common value for speed and offspring of a fish
    SPEED = 10
    SPAWN = 2
      
    def __init__(self, x, y, size):
        """ Initialize fish sprite. """
        #Takes value of x, y and size from main func
        super(Fish, self).__init__(
            image = Fish.images[size],
            x = x, y = y,
            dx = random.choice([1, -1]) * Fish.SPEED * random.random()/size, 
            dy = random.choice([1, -1]) * Fish.SPEED * random.random()/size)
        
        self.size = size

    def update(self):
        """ Wrap around screen. """    
        if self.top > games.screen.height:
            self.bottom = 0
 
        if self.bottom < 0:
            self.top = games.screen.height

        if self.left > games.screen.width:
            self.right = 0

        if self.right < 0:
            self.left = games.screen.width



    def die(self):
        """ Destroy fish. """
        # if fish isn't smallest, replace with two smaller fishes
        if self.size != Fish.SMALL:
            for i in range(Fish.SPAWN):
                
                new_fish = Fish(x = random.randrange(games.screen.width),
                                        y = random.randrange(games.screen.height),
                                        size = self.size - 1)
                games.screen.add(new_fish)
        self.destroy()

    



class Shark(games.Sprite):
    """ The player's shark. """
    image = games.load_image("Shark.bmp")

    #Rotation step and velocity steps are used in seperate funcs below
    ROTATION_STEP = 20
    VELOCITY_STEP = 5
    

    def __init__(self, x, y):
        """ Initialize ship sprite. """
        super(Shark, self).__init__(image = Shark.image, x = x, y = y)
        self.missile_wait = 0
        self.angle = 90
        

        self.score = games.Text(value = 0, size = 25, color = color.black,
                                top = 5, right = games.screen.width - 10)
        self.total = games.Text(value = 8, size = 25, color = color.black, top = 5, left = 0)
        games.screen.add(self.score)
        self.start_game()
        #games.screen.add(self.death)


    def update(self):
        """ Rotate and thrust based on keys pressed. """
        # rotate based on left and right arrow keys
        if games.keyboard.is_pressed(games.K_LEFT):
            self.angle -= Shark.ROTATION_STEP        
        if games.keyboard.is_pressed(games.K_RIGHT):
            self.angle += Shark.ROTATION_STEP

        # apply thrust based on up arrow key
        if games.keyboard.is_pressed(games.K_UP):
            # change velocity components based on ship's angle
            angle = self.angle * math.pi / 180  # convert to radians
            self.dx += Shark.VELOCITY_STEP* math.sin(angle)
            self.dy += Shark.VELOCITY_STEP* -math.cos(angle)

        if games.keyboard.is_pressed(games.K_DOWN):
            #stop the shark from thrusting forward
            self.dx = self.dy = 0


        # wrap the shark around screen    
        if self.top > games.screen.height:
            self.bottom = 0
 
        if self.bottom < 0:
            self.top = games.screen.height

        if self.left > games.screen.width:
            self.right = 0

        if self.right < 0:
            self.left = games.screen.width    

        # check if ship overlaps any other object
        if self.overlapping_sprites :
            for sprite in self.overlapping_sprites:
                #every sprite except score and death counter are destroyed on contact
                if sprite != self.score and sprite != self.total:
                   sprite.die()
                   #Adds 10 to the score
                   self.score.value += 10
                   self.total.value -= 1
                   if sprite.size != Fish.SMALL:
                       self.total.value += 2
                   if self.total.value == 0:
                       self.die()
                       self.end_game()
                   
                   self.score.right = games.screen.width - 10 
        

    def die(self):
        """ Destroy shark. """
        self.destroy()

    
    def end_game(self):
        """ End the game. """
        end_message = games.Message(value = "Game Over. Final Score: ",
                                    size = 90,
                                    color = color.red,
                                    x = games.screen.width/2,
                                    y = games.screen.height/2-90,
                                    lifetime = 20,
                                    after_death = games.screen.quit)
        
        end_message_2 = games.Message(value = self.score.value,
                                    size = 90,
                                    color = color.red,
                                    x = games.screen.width/2,
                                    y = games.screen.height/2,
                                    lifetime = 20,
                                    after_death = games.screen.quit)
        games.screen.add(end_message)
        games.screen.add(end_message_2)

    def start_game(self):
          start_message = games.Message(value = "Deadly Shark Attack"
                                        "Press arrow keys to move",
                                        size = 50,
                                        color = color.red,
                                        x = games.screen.width/2,
                                        y = games.screen.height/2,
                                        lifetime = 5,
                                        )
          games.screen.add(start_message)


          start_message = games.Message(value = "Deadly Shark Attack"
                                        "Press arrow keys to move",
                                        size = 50,
                                        color = color.red,
                                        x = games.screen.width/2,
                                        y = games.screen.height/2,
                                        lifetime = 5,
                                        )
          games.screen.add(start_message)
           
        
   


def main():
    # establish background
    nebula_image = games.load_image("Tank.jpg")
    games.screen.background = nebula_image

    # create 8 fishes
    for i in range(8):
        x = random.randrange(games.screen.width)
        y = random.randrange(games.screen.height)
        size = random.choice([Fish.SMALL, Fish.MEDIUM, Fish.LARGE])
        new_fish = Fish(x = x, y = y, size = size)
        games.screen.add(new_fish)


    
    # create the shark
    the_shark = Shark(x = 50, y = 50)
    games.screen.add(the_shark)
        
    games.screen.mainloop()

# kick it off!
main()

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
