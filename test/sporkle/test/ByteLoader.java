package sporkle.test;

public class ByteLoader
  extends ClassLoader
{

  // public ByteLoader() { super(); }

  public Class loadBytes(byte[] bytes) {
    return defineClass(bytes, 0, bytes.length);
  }
}

