package lslplus.debug;

import java.util.List;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.converters.SingleValueConverter;
import com.thoughtworks.xstream.converters.basic.FloatConverter;
import com.thoughtworks.xstream.converters.basic.IntConverter;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class LslScriptExecutionState {

    public static abstract class Value { 
        public abstract String typeString();
    }
    public static class IntegerValue extends Value {
        private int val;
        public IntegerValue(int i) {
            val = i;
        }
        public int getVal() { return val; }
        public String toString() { return Integer.toString(val); }
        public String typeString() { return "integer"; }
    }
    
    public static class FloatValue extends Value {
        private float val;
        public FloatValue(float f) {
            val = f;
        }
        public float getVal() { return val; }
        public String toString() { return Float.toString(val); }
        public String typeString() { return "float"; }
    }
    
    public static class StringValue extends Value {
        private String val;
        public StringValue(String s) {
            val = s;
        }
        public String getVal() { return val; }
        
        // TODO FIX!!!
        public String toString() { return val; } 
        public String typeString() { return "string"; }
    }
   
    public static class KeyValue extends Value {
        private String val;
        public KeyValue(String s) {
            val = s;
        }
        public String getVal() { return val; }
        // TODO FIX!!!
        public String toString() { return val; } 
        public String typeString() { return "key"; }
    }
    
    public static class VectorValue extends Value {
        private float x,y,z;
        public VectorValue(float f1, float f2, float f3) {
            x = f1;
            y = f2;
            z = f3;
        }
        
        public float getX() { return x;}
        public float getY() { return y;}
        public float getZ() { return z;}
        
        public String toString() {
            return "<" + x + "," + y + "," + z + ">";
        }
        public String typeString() { return "vector"; }
    }

    public static class RotationValue extends Value {
        private float x,y,z,s;
        public RotationValue(float f1, float f2, float f3, float f4) {
            x = f1;
            y = f2;
            z = f3;
            s = f4;
        }
        
        public float getX() { return x;}
        public float getY() { return y;}
        public float getZ() { return z;}
        public float getS() { return s;}
        
        public String toString() {
            return "<" + x + "," + y + "," + z + "," + s + ">";
        }
        public String typeString() { return "rotation"; }
    }
    
    public static class ListValue extends Value {
        private Value[] elements = null;
        public ListValue(List l) {
            elements = (Value[]) l.toArray(new Value[0]);
        }
        
        public Value[] getElements() { return elements; }
        
        public String toString() {
            StringBuilder buf = new StringBuilder("[");
            String sep = "";
            if (elements == null) return "[]";
            for (int i = 0; i < elements.length; i++) {
                buf.append(sep);
                buf.append(elements[i].toString());
                sep = ",";
            }
            
            buf.append("]");
            return buf.toString();
        }
        public String typeString() { return "list"; }
    }
    
    public static class Binding {
        private String name;
        private Value val;
        public Binding(String name, Value val) {
            this.name = name;
            this.val = val;
        }
        
        public String getName() { return name; }
        public Value getVal() { return val; }
    }
    
    public static class Frame {
        private String name = null;
        private String file = null;
        private Integer line = null;
        private Binding[] bindings;
        public Frame(Binding[] bindings) {
            this.bindings = bindings;
        }
        
        public Binding[] getBindings() { return bindings; }
        public String getFile() { return file; }
        public Integer getLine() { return line; }
        public String getName() { return name; }
    }
    
    private String sourceElement;
    private int currentLine;
    private Frame[] frames;
    
    public String getSourceElement() { return sourceElement; }
    public int getCurrentLine() { return currentLine; }
    public Frame[] getFrames() { return frames; }

    private static XStream xstream;
    
    static {
        xstream = new XStream(new DomDriver());
        configureXStream(xstream);
    }
    public static void configureXStream(XStream xstream) {
        xstream.alias("script-state", LslScriptExecutionState.class);
        xstream.alias("frame", Frame.class);
        xstream.alias("binding", Binding.class);
        xstream.aliasType("integer-value",IntegerValue.class);
        xstream.aliasType("float-value", FloatValue.class);
        xstream.aliasType("string-value", StringValue.class);
        xstream.aliasType("key-value", KeyValue.class);
        xstream.aliasType("vector-value", VectorValue.class);
        xstream.aliasType("rotation-value", RotationValue.class);
        xstream.aliasType("list-value", ListValue.class);
        xstream.alias("value", Value.class);
        xstream.registerConverter(new SingleValueConverter() {
            private FloatConverter conv = new FloatConverter();
            public boolean canConvert(Class arg0) {
                return FloatValue.class.equals(arg0);
            }
            public Object fromString(String arg0) {
                return new FloatValue(((Float)conv.fromString(arg0)).floatValue());
            }
            public String toString(Object arg0) {
                return arg0.toString();
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            private IntConverter conv = new IntConverter();
            public Object fromString(String arg0) {
                
                return new IntegerValue(((Integer)conv.fromString(arg0)).intValue());
            }

            public String toString(Object arg0) {
                return conv.toString(arg0.toString());
            }

            public boolean canConvert(Class arg0) {
                return IntegerValue.class.equals(arg0);
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            public Object fromString(String arg0) {
                
                return new StringValue(arg0);
            }

            public String toString(Object arg0) {
                return ((StringValue)arg0).val;
            }

            public boolean canConvert(Class arg0) {
                return StringValue.class.equals(arg0);
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            public Object fromString(String arg0) {
                
                return new KeyValue(arg0);
            }

            public String toString(Object arg0) {
                return ((KeyValue)arg0).val;
            }

            public boolean canConvert(Class arg0) {
                return KeyValue.class.equals(arg0);
            }
            
        });
    }
    
    public static LslScriptExecutionState fromXML(String xml) {
        return (LslScriptExecutionState) xstream.fromXML(xml);
    }
    
}