
let rec digitsOfInt n =
  if n <= 0
  then []
  else
    if (n mod 10) = 0
    then 0 :: (digitsOfInt (n / 10))
    else
      if ((n - 1) mod 10) = 0
      then 1 :: (digitsOfInt ((n - 1) / 10))
      else
        if ((n - 2) mod 10) = 0
        then 2 :: (digitsOfInt ((n - 2) / 10))
        else
          if ((n - 3) mod 10) = 0
          then 3 :: (digitsOfInt ((n - 3) / 10))
          else
            if ((n - 4) mod 10) = 0
            then 4 :: (digitsOfInt ((n - 4) / 10))
            else
              if ((n - 5) mod 10) = 0
              then 5 :: (digitsOfInt ((n - 5) / 10))
              else
                if ((n - 6) mod 10) = 0
                then 6 :: (digitsOfInt ((n - 6) / 10))
                else
                  if ((n - 7) mod 10) = 0
                  then 7 :: (digitsOfInt ((n - 7) / 10))
                  else
                    if ((n - 8) mod 10) = 0
                    then 8 :: (digitsOfInt ((n - 8) / 10))
                    else
                      if ((n - 9) mod 10) = 0
                      then 9 :: (digitsOfInt ((n - 9) / 10));;
