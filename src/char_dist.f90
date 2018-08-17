  subroutine distval(values, vecLen, icd)

  implicit real*8(A-H,O-Z)
  integer :: icd(6), bins(6), vecLen, i = 1, j, sumVec
  integer :: values(vecLen), bVal
  logical :: whichBin = .true.
  integer, allocatable :: newVec(:), zeros(:), nonzeros(:)

  allocate(newVec(vecLen - 1))
  bins = (/ 1, 4, 7, 12, 16, 256 /)

  ! Initialise variables
  bVal = 2
  newVec = values(2:vecLen)
  sumVec = sum(newVec)

  ! Figure out how to allocate the bins
  do while (whichBin)
    if (bins(i) > sumVec) then
      whichBin = .false.
    else
      i = i + 1
    end if
  end do

  if (vecLen == 1) then
    icd = (/ values(1), 0, 0, 0, 0, 0 /)
  else
    allocate(nonzeros(i - 1))
    allocate(zeros(6 - i))
    zeros = 0
    nonzeros = 0
    do j = 1,(vecLen - 1)
      nonzeros(bVal - 1) = nonzeros(bVal - 1) + values(j + 1)
      if ((j + 1) >= bins(bVal)) then
        bVal = bVal + 1
      end if
    end do
    icd = (/ values(1), nonzeros, zeros /)

    ! Must always deallocate vectors
    deallocate(nonzeros)
    deallocate(zeros)

  end if

  end subroutine distval
