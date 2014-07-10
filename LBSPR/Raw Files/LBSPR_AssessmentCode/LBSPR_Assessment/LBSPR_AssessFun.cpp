#include <admodel.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <LBSPR_AssessFun.htp>

model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
  MK.allocate("MK");
  Linf.allocate("Linf");
  LinfCV.allocate("LinfCV");
  PercentLeft.allocate("PercentLeft");
  NumAge.allocate("NumAge");
  RelAge.allocate(1,NumAge,"RelAge");
  LBins.allocate("LBins");
  LengthMids.allocate(1,LBins,"LengthMids");
  LengthClss.allocate(1,LBins+1,"LengthClss");
  ObsLength.allocate(1,LBins,"ObsLength");
  ObsProp.allocate(1,LBins,"ObsProp");
  MatL50.allocate("MatL50");
  MatL95.allocate("MatL95");
 cout << "Done Data" << endl;
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  initializationfunction();
  SelL50.allocate(0,1,1,"SelL50");
  Delta.allocate(0.00001,1,1,"Delta");
  logFM.allocate(2,"logFM");
  AgeLengthProbMat.allocate(1,LBins,1,NumAge,"AgeLengthProbMat");
  #ifndef NO_AD_INITIALIZE
    AgeLengthProbMat.initialize();
  #endif
  Lengths.allocate(1,NumAge,"Lengths");
  #ifndef NO_AD_INITIALIZE
    Lengths.initialize();
  #endif
  LenSD.allocate(1,NumAge,"LenSD");
  #ifndef NO_AD_INITIALIZE
    LenSD.initialize();
  #endif
  AgeVec.allocate(1,NumAge,"AgeVec");
  #ifndef NO_AD_INITIALIZE
    AgeVec.initialize();
  #endif
  SelA.allocate(1,NumAge,"SelA");
  #ifndef NO_AD_INITIALIZE
    SelA.initialize();
  #endif
  SelL.allocate(1,LBins,"SelL");
  #ifndef NO_AD_INITIALIZE
    SelL.initialize();
  #endif
  Pop.allocate(1,NumAge,"Pop");
  #ifndef NO_AD_INITIALIZE
    Pop.initialize();
  #endif
  CatchatAge.allocate(1,NumAge,"CatchatAge");
  #ifndef NO_AD_INITIALIZE
    CatchatAge.initialize();
  #endif
  CatchatLength.allocate(1,LBins,"CatchatLength");
  #ifndef NO_AD_INITIALIZE
    CatchatLength.initialize();
  #endif
  CatchatLength2.allocate(1,LBins,"CatchatLength2");
  #ifndef NO_AD_INITIALIZE
    CatchatLength2.initialize();
  #endif
  FM.allocate("FM");
  #ifndef NO_AD_INITIALIZE
  FM.initialize();
  #endif
  ZVector.allocate(1,NumAge,"ZVector");
  #ifndef NO_AD_INITIALIZE
    ZVector.initialize();
  #endif
  ZVector2.allocate(1,NumAge,"ZVector2");
  #ifndef NO_AD_INITIALIZE
    ZVector2.initialize();
  #endif
  FVector.allocate(1,NumAge,"FVector");
  #ifndef NO_AD_INITIALIZE
    FVector.initialize();
  #endif
  FMVector.allocate(1,NumAge,"FMVector");
  #ifndef NO_AD_INITIALIZE
    FMVector.initialize();
  #endif
  SelAgeLengthProbs.allocate(1,LBins,1,NumAge,"SelAgeLengthProbs");
  #ifndef NO_AD_INITIALIZE
    SelAgeLengthProbs.initialize();
  #endif
  genF.allocate("genF");
  #ifndef NO_AD_INITIALIZE
  genF.initialize();
  #endif
  genM.allocate("genM");
  #ifndef NO_AD_INITIALIZE
  genM.initialize();
  #endif
  CStructPred.allocate(1,LBins,"CStructPred");
  #ifndef NO_AD_INITIALIZE
    CStructPred.initialize();
  #endif
  CStructPred2.allocate(1,LBins,"CStructPred2");
  #ifndef NO_AD_INITIALIZE
    CStructPred2.initialize();
  #endif
  Zscore.allocate("Zscore");
  #ifndef NO_AD_INITIALIZE
  Zscore.initialize();
  #endif
  NormLengths.allocate(1,LBins,"NormLengths");
  #ifndef NO_AD_INITIALIZE
    NormLengths.initialize();
  #endif
  LengthClasses.allocate(1,LBins+1,"LengthClasses");
  #ifndef NO_AD_INITIALIZE
    LengthClasses.initialize();
  #endif
  SelNLL.allocate("SelNLL");
  #ifndef NO_AD_INITIALIZE
  SelNLL.initialize();
  #endif
  SumOfAges.allocate("SumOfAges");
  NewStandProbMat.allocate(1,LBins,1,NumAge,"NewStandProbMat");
  #ifndef NO_AD_INITIALIZE
    NewStandProbMat.initialize();
  #endif
  SPR.allocate("SPR");
  #ifndef NO_AD_INITIALIZE
  SPR.initialize();
  #endif
  obj_fun.allocate("obj_fun");
 cout << "Done Parameters" << endl;
}

void model_parameters::userfunction(void)
{
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
}

void model_parameters::AgeLengthProbFun(void)
{
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
}

void model_parameters::Select(void)
{
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
}

void model_parameters::PredictLengthStruct(void)
{
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
}

void model_parameters::EstimateSPR(void)
{
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
}

void model_parameters::report()
{
 adstring ad_tmp=initial_params::get_reportfile_name();
  ofstream report((char*)(adprogram_name + ad_tmp));
  if (!report)
  {
    cerr << "error trying to open report file"  << adprogram_name << ".rep";
    return;
  }
 report << CStructPred << endl; // EstimatedProp 
 report <<  ObsProp << endl; // ObservedProp 
 report <<  obj_fun << endl; // objective function value
 report <<  SPR << endl; // SPR
}

void model_parameters::preliminary_calculations(void){
  admaster_slave_variable_interface(*this);
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{}

void model_parameters::final_calcs(void){}

void model_parameters::set_runtime(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

  long int arrmblsize=0;

int main(int argc,char * argv[])
{
    ad_set_new_handler();
  ad_exit=&ad_boundf;
 arrmblsize = 500000000;
 gradient_structure::set_GRADSTACK_BUFFER_SIZE(1.e7);
 gradient_structure::set_CMPDIF_BUFFER_SIZE(1.e7);
 gradient_structure::set_MAX_NVAR_OFFSET(5000);
 gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);
  
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
  #if defined(__GNUDOS__) || defined(DOS386) || defined(__DPMI32__)  || \
     defined(__MSVC32__)
      if (!arrmblsize) arrmblsize=150000;
  #else
      if (!arrmblsize) arrmblsize=25000;
  #endif
    model_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;
    mp.preliminary_calculations();
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}
