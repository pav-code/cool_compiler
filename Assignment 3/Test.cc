    test=0;
    for (it=InhTree.begin(); it!=InhTree.end(); ++it) {
       test=test + 10;
       bNext = false;
       if ( strcmp(it->first, "IO") == 0 || 
            strcmp(it->first, "Int") == 0 ||
            strcmp(it->first, "String") == 0 ||
            strcmp(it->first, "Bool") == 0 ||
            strcmp(it->first, "Object") == 0 ) {
          bNext = true;
       }
       if (!bNext) {
         CycleTree.clear();
         CycleTree.insert(std::pair<char*,char*>(it->second.Get_Name_String(),it->second.Get_Parent_String()));
         cit = CycleTree.begin();
         while (true) {  
             if ( strcmp(cit->second, "IO") == 0 || 
                  strcmp(cit->second, "Object") == 0 ) {
                 break;
             }
             if ( strcmp(cit->second, "Int") == 0 || 
                  strcmp(cit->second, "String") == 0 ||
                  strcmp(cit->second, "Bool") == 0 ) {
                 //TODO: Error - Inheritance from Int, String, or Bool
                 myfile << "Error: Attempt to inherit Int, String or Bool" << endl;
                 break;
             }
             if (CycleTree.find(cit->second) == CycleTree.end()) {
                 dummyit = InhTree.find(cit->second);
                 CycleTree.insert(std::pair<char*,char*>(dummyit->second.Get_Name_String(),dummyit->second.Get_Parent_String()));  
                 cit = CycleTree.find(dummyit->second.Get_Name_String());   
             } else {
                 //TODO: Error - Cyclic Inheritance
                 myfile << "Error: Cyclic inheritance on class" << cit->second << endl;
                 break;
             }

             for (cittest=CycleTree.begin(); cittest!=CycleTree.end(); ++cittest)
             {
                 myfile << "#" << test << " :";
                 myfile << "Name: " << cittest->first;
                 myfile << " Parent: " << cittest->second << endl;
             }
             test++;  
         }
      }
    }