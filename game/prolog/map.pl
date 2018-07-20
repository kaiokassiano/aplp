:-dynamic pacman/3. 
:-dynamic ghost/3. 
:-dynamic cherry/2.  

grid([[1,1,1,3,1,1,1], 
      [1,0,0,0,0,0,1], 
      [1,0,1,1,1,0,1], 
      [2,0,0,0,0,0,2], 
      [1,0,1,0,1,0,1], 
      [1,0,0,0,0,0,1], 
      [1,1,1,3,1,1,1]]).

pacman(2,2,normal). 
ghost(6,6,'Red'). 
ghost(4,6,'Red'). 
cherry(2,6).
