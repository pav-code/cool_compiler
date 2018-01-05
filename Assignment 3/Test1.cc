    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ classObj = classes->nth(i);
        classtable -> print_class(classObj);
    }




void ClassTable::print_class(Class_ pClass)
{
    ofstream myfile;
    myfile.open("example.txt", std::ofstream::app);
    Features pFeatures = pClass->get_features();
    Feature pFeature;

int i = pFeatures->next(155);
    for (int i = pFeatures->first(); pFeatures->more(i); pFeatures->next(i) ) {
//        pFeature = pFeatures->nth(i);
//        if ( strcmp(pFeature->feature_type(), "method") == 0 ) {
//            myfile << "method: " << pFeature->get_name() << "( ";
//            print_formals(pFeature->get_formals(), myfile);
//            myfile << " ). Return Type: " << pFeature->get_return_type() << endl;
//        } else if ( strcmp(pFeature->feature_type(), "attr") == 0 ) {
//            myfile << "attr: " << pFeature->get_name() << " : " << pFeature->get_type() << endl;
//        }
    }

    myfile.close();
}

void ClassTable::print_formals(Formals pFormals, ofstream& myfile)
{
    Formal pFormal;
    for (int i = pFormals->first(); pFormals->more(i); pFormals->next(i)) {
        pFormal = pFormals->nth(i);
        myfile << "Formal-" << pFormal->get_name() << " : " << pFormal->get_type() << "," << endl;
    }
}