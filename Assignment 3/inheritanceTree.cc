#include <iostream>
#include <map>

class Graph_Node
{
    private:
	   Graph_Node* nodeParent;
	   char* sName;
	   char* sParent;
	
	public:
	    Graph_Node(char* Name, char* Parent): sName(Name), sParent(Parent) {}
		Graph_Node* Get_Parent();
        void Set_Parent(Graph_Node* );
		char* Get_Name_String();
		char* Get_Parent_String();
};

Graph_Node* Graph_Node::Get_Parent()
{
   return nodeParent;
}

char* Graph_Node::Get_Name_String()
{
    return sName;
}

char* Graph_Node::Get_Parent_String()
{
    return sParent;
}

void Graph_Node::Set_Parent(Graph_Node* parent)
{
    nodeParent = parent;
}

int main()
{
    std::map<int, Graph_Node> InhTree;
    std::map<int, Graph_Node>::iterator it;
    InhTree.insert(std::pair<int,Graph_Node>(0,Graph_Node("A","Object")));
    InhTree.insert(std::pair<int,Graph_Node>(1,Graph_Node("B","Object")));
    InhTree.insert(std::pair<int,Graph_Node>(2,Graph_Node("C","A")));
//Graph_Node test8 = Graph_Node("A","Object");
//InhTree[0];
    it=InhTree.find(0);
    //it->second->Get_Name();
	
   return 0;
}