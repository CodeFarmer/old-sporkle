import java.util.List;
import java.util.HashMap;

class ClassWithAllConstantPoolTypes
{

  int i = 102401;
  float f = 120.0f;
  long l = 3L;
  double d = 4.0D;

  String s = "everybody loves string";

  int countListAndHashMap(List list, HashMap hashmap) {
    int x = list.size(); // interfacemethod
    int y = hashmap.keySet().size(); // concrete method
    return x + y;
  }

}
