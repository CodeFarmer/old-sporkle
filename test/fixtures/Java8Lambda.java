import java.util.Arrays;

class Java8Lambda
{

    public static void main(String[] argv) {

	Arrays.stream(new String[]{"a", "b", "c"})
	    .forEach(s -> System.out.println(s));

    }

}
