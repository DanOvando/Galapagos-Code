// TPL file for LBSPR estimation procedure

DATA_SECTION
  init_number MK;
  init_number Linf;
 // init_number MeanSelL50;
//  init_number SDSelL50;
  init_number LinfCV;
  init_number PercentLeft;						  // Percent of animals left at max age
  init_number NumAge; 							  // Number of age classes
  init_vector RelAge(1,NumAge);                   // Vector of relative age classes
  init_int LBins;                                 // Number of Length Bins
  init_vector LengthMids(1,LBins);             	  // Length Mids
  init_vector LengthClss(1,LBins+1);			  // Lenght Classess
  init_vector ObsLength(1,LBins);                 // Number of lengths in each bin
  init_vector ObsProp(1,LBins);                   // Proportion of lengths in each bin
  init_number MatL50;
  init_number MatL95;
  

 !! cout << "Done Data" << endl;
 
// ----------------------------------------------------------------------------
 PARAMETER_SECTION  
  init_bounded_number SelL50(0,1,1);           //
  init_bounded_number Delta(0.00001,1,1);	
  init_number logFM(2);  // 
  // init_bounded_number logMK(MKLower,MKUpper,1);
  // init_bounded_number MK(1.5,1.65,1);
  // init_bounded_number logLinf(LinfLower,LinfUpper,1);
  
  // Storage
  matrix AgeLengthProbMat(1,LBins,1,NumAge);
  vector Lengths(1,NumAge);
  vector LenSD(1,NumAge);
  vector AgeVec(1,NumAge);
  vector SelA(1,NumAge);                              // Storage:Selectivity-at-age
  vector SelL(1,LBins);                               // Storage:Selectivity-at-age
  vector Pop(1,NumAge);                               // Storage:Pop at age
  vector CatchatAge(1,NumAge);                        // Storage:Catch at age
  vector CatchatLength(1,LBins);                      // Storage:Catch at length
  vector CatchatLength2(1,LBins);
  number FM;                                   	      // storage: FM when delogged
  vector ZVector(1,NumAge);			      			  // storage: vector of mortalities
  vector ZVector2(1,NumAge);
  vector FVector(1,NumAge);
  vector FMVector(1,NumAge);			      		  // storage: vector of mortalities
  matrix SelAgeLengthProbs(1,LBins,1,NumAge);	      // global because used in both functions
  number genF;                                   	  //  
  number genM;  
  vector CStructPred(1,LBins);			              //Predicted catch structure	
  vector CStructPred2(1,LBins);
  number Zscore;
  vector NormLengths(1,LBins);
  vector LengthClasses(1,LBins+1);
  number SelNLL;
  vector SumOfAges;
  matrix NewStandProbMat(1,LBins,1,NumAge);
  
  number SPR;
  
 // number LinfVar;
 // sdreport_number LinfVar;
 
  objective_function_value obj_fun;
  
  !! cout << "Done Parameters" << endl;
  
// ----------------------------------------------------------------------------
//INITIALIZATION_SECTION
 //SelL50 0.2
 //Delta 0.2  // changed from SelL95 to Delta
 //logFM  0 //-1.8

// ---------------------------------------------------------------------------- 
PROCEDURE_SECTION
 
  //SelNLL = square(SelL50+(MeanSelL50/Linf))/(2*(SDSelL50/Linf)*(SDSelL50/Linf));
  dvar_vector SelL(1,LBins);
  dvar_vector SelA(1,NumAge);

  
  NormLengths = LengthMids/Linf;
  LengthClasses = LengthClss/Linf;
  
  AgeLengthProbFun();
  
  
  // Set up the Sel at length and Sel at age pattern
  Select();
  
 // Project the model forward and compute various outputs
  PredictLengthStruct(); 
  
  EstimateSPR();
  
  obj_fun = sum(elem_prod(ObsLength, log(elem_div((ObsProp+0.000001),(CStructPred+0.000001)))));
  
// ---------------------------------------------------------------------------- 
FUNCTION AgeLengthProbFun
  int Age;
  int Len;

  
  Lengths = 1.0-pow(PercentLeft, RelAge * (1/MK));
  LenSD = LinfCV*Lengths;
  
  AgeVec(1) = 1;
  for (Age=2;Age<=NumAge;Age++) {
    AgeVec(Age) = AgeVec(Age-1) + 1;
  }  

  for (Age=1;Age<=NumAge;Age++) {
    Zscore = (LengthClasses(1) - Lengths(Age))/LenSD(Age);
    AgeLengthProbMat(1,Age) = cumd_norm(Zscore);
  }
  
  for (Age=1;Age<=NumAge;Age++) {
    for (Len=3;Len<=LBins+1;Len++) {
	  Zscore = (LengthClasses(Len) - Lengths(Age))/LenSD(Age);
	  AgeLengthProbMat(Len - 1,Age) =  cumd_norm(Zscore) - cumd_norm((LengthClasses(Len-1) - Lengths(Age))/LenSD(Age));
    }
  }
  
 // Calculate Sel at age, convert it to selectivity at length
FUNCTION Select
  int Length,Age;
  
  for (Length=1;Length<=LBins;Length++) {
    SelL(Length) = 1.0/(1+exp(-log(19)*(NormLengths(Length)-SelL50)/((SelL50+Delta)-SelL50)));  // changed to Delta
  }
   
  for (Age=1;Age<=(NumAge);Age++) {
    for (Length=1;Length<=LBins;Length++) {
      SelAgeLengthProbs(Length,Age) = AgeLengthProbMat(Length,Age) * SelL(Length);
    }
  }
  
  
  // need to normalise SelAgeLengthProbs still
  SumOfAges = colsum(SelAgeLengthProbs);
  for (Age=1;Age<=NumAge;Age++) {
    for (Length=1;Length<=LBins;Length++) {
      NewStandProbMat(Length,Age) = SelAgeLengthProbs(Length,Age)/SumOfAges(Age);
    }
  }
    
  // Selectivity at age
  SelA = SelL * AgeLengthProbMat; 

 //cout << "Selectivity = " << SelA << endl;
  
// ---------------------------------------------------------------------------- 
FUNCTION PredictLengthStruct
  int Length,Age;

 // Compute vector of mortalities
  FM = mfexp(logFM);
  FMVector = FM  * SelA;
  
  genM = (-log(PercentLeft))/NumAge;
  genF = FM * genM;
  FVector = genF * SelA;
  ZVector = FVector + genM;

  for (Age=1;Age<=NumAge;Age++) { 
    if (Age == 1) Pop(Age) = 1E5;
    if (Age > 1 ) { 
	  Pop(Age) = Pop(Age-1)*exp(-ZVector(Age-1));
	}
  }
   
   CatchatLength = elem_prod(Pop, SelA) * trans(NewStandProbMat);

  for (Length=1;Length<=LBins;Length++) {
    CStructPred(Length) = CatchatLength(Length)/sum(CatchatLength);
  }
 
FUNCTION EstimateSPR
  int Length,Age;
  dvariable matL50;
  dvariable matL95;
  dvar_vector MatL(1,LBins);
  dvar_vector MatA(1,NumAge);
  dvar_vector E(1,NumAge);
  dvar_vector UnF(1,NumAge);
  dvar_vector Fished(1,NumAge);
  dvariable UnfishedEgg;
  dvariable FishedEgg;
  
  matL50 = MatL50/Linf;
  matL95 = MatL95/Linf;
  
  for (Length=1;Length<=LBins;Length++) {
    MatL(Length) = 1.0/(1+exp(-log(19)*(NormLengths(Length)-matL50)/(matL95-matL50)));  //
  }
  
  MatA = MatL * AgeLengthProbMat; 
  E = elem_prod(MatA, pow(Lengths,3));
  
  for (Age=1;Age<=NumAge;Age++) { 
    if (Age == 1) UnF(Age) = 1E5;
    if (Age > 1 ) { 
	  UnF(Age) = UnF(Age-1)*exp(-genM);
	} 
  }
  
  for (Age=1;Age<=NumAge;Age++) { 
    if (Age == 1) Fished(Age) = 1E5;
    if (Age > 1 ) { 
	  Fished(Age) = Fished(Age-1)*exp(-ZVector(Age-1));
	} 
  }
   FishedEgg = sum(elem_prod(Fished, E));
   UnfishedEgg = sum(elem_prod(UnF, E));
   SPR = FishedEgg/UnfishedEgg;
  
 
// ---------------------------------------------------------------------------- 
REPORT_SECTION
 report << CStructPred << endl; // EstimatedProp 
 report <<  ObsProp << endl; // ObservedProp 
 report <<  obj_fun << endl; // objective function value
 report <<  SPR << endl; // SPR

 
 
// ---------------------------------------------------------------------------- 
TOP_OF_MAIN_SECTION
 arrmblsize = 500000000;
 gradient_structure::set_GRADSTACK_BUFFER_SIZE(1.e7);
 gradient_structure::set_CMPDIF_BUFFER_SIZE(1.e7);
 gradient_structure::set_MAX_NVAR_OFFSET(5000);
 gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);
  
