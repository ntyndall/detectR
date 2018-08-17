  subroutine maxinter(vec, vLen, currentInd)

  implicit real*8(A-H,O-Z)
  integer :: j, currentInd, currentSum = 0, subSum, vLen, subsetVal = 4
  double precision :: vec(1:vLen)

  ! Loop over finding the max score within e.g. [1, 2, 3, 4, 5]
  do j = 1, (vLen - subsetVal)
    subSum = sum(vec(j:(j + subsetVal)))

    ! Track the current index
    if (subSum > currentSum) then
      currentInd = j
      currentSum = subSum
    end if
  end do

  end subroutine maxinter
