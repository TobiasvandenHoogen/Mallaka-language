@ This file serves as an example for quicksorting in the Mallaka Language

@ Create list for sorting (you can also create your own)
lst = [1, 10, 4, 9, 3, 5, 30, 1];

@ Get the start index of the list
lw = 0;

@ Get the final index of the list 
hg = carrot lst - 1;

@ Function to handle a partition of the list 
process partition {lst, low, high} ->
  @ Get the pivot of the list 
  pivot = lst $ high;

  @ Pointer for the greater element 
  i = (low - 1);

  @ Pointer for the first element 
  j = low;

  @ Loop through each index of the list within the partition 
  loop from j to (high - 1) with 1 ->
    @ If the element is equal or smaller than the pivot 
    whatif((lst $ j) <= pivot)->
      @ Swap the position with the greater element
      i = i + 1;
      tempI = lst $ i;
      (lst $ i) = (lst $ j);
      (lst $ j) = tempI;
      <-;
     @ Go to the next element
     j = j + 1;
    <-;

  @ Swap the pivot with the greater element 
  tempA = lst $ (i + 1);
  (lst $ (i + 1)) = (lst $ high);
  (lst $ high) = tempA;

  @ Return a list which contains the partition position and the new list 
  return = [(i + 1), lst];
<-; 

@ Function that sorts a list with the quicksort algorithm 
process quickSort {list, sIndex, fIndex} ->
  @ If the list hasn't reached it left or right limit 
  whatif(sIndex < fIndex) ->
    @ Partition the list 
    par = run partition{list, sIndex, fIndex}; 

    @ Get the partition position 
    parIndex = (par $ 0);

    @ Get the list where the partition took place
    parList = (par $ 1);

    @ Recursive call on the left side of the pivot 
    lft = run quickSort{parList, sIndex, (parIndex - 1)};

    @ Recursive call on the right side of the pivot 
    rgt = run quickSort{lft, (parIndex + 1), fIndex};

    @ Return the sorted list 
    rgt;
    <-;
<-;
