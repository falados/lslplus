package lslplus.lsltest;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import lslplus.LslProjectNature;
import lslplus.language_metadata.LslParam;
import lslplus.util.Util;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class LslTest {

	private static final String EMPTY_STRING = "\"\""; //$NON-NLS-1$
    private static final String BLANK = ""; //$NON-NLS-1$
	public static int MODULE_TEST = 0;
	public static int HANDLER_TEST = 1;
	public static int SCRIPT_FUNCTION_TEST = 2;
	public String name = BLANK;
	private EntryPoint entryPoint = null;
	public LslValue[] arguments = null;
	private MaybeValue expectedReturn = null;
	private CallExpectations expectations = new CallExpectations();
	private ArrayList initialBindings = new ArrayList();
	private ArrayList finalBindings = new ArrayList();
    private LslTestSuite suite;
	
	public static class LslValue {
	    private String mnemonic = null;
	    public String getMnemonic() { return mnemonic; }
	    public void setMnemonic(String mnemonic) { this.mnemonic = mnemonic; }
	}
	
	public static class MaybeValue {
	    private LslValue val = null;
	    private Class type = null;
	    
	    public MaybeValue() {
	        this.val = null;
	    }
	    public MaybeValue(LslValue val) {
	        this.val = val;
        }

        public LslValue getVal() { return val; }
	    
	    public Class getType() { return type; }
	    
	    public String toString() {
	        return val == null ? BLANK : val.toString();
	    }

        public void setVal(LslValue o) {
            this.val = o;
        }
	}
	
	public static class LslVoid extends LslValue {
		public String toString() { return BLANK; }
	}

	public static class LslString extends LslValue {
		public String val;

		public LslString(String val) {
			this.val = val;
		}
		
		public String toString() {
			return val;
		}

	}
	
	public static class LslInteger extends LslValue {
		public String val;

		public LslInteger(String val) {
			this.val = val;
		}
		
		public String toString() {
			return val;
		}
	}
	
	public static class LslFloat extends LslValue {
		public String val;
		
		public LslFloat(String val) {
			this.val = val;
		}
		
		public String toString() {
			return val;
		}
	}
	
	public static class LslKey extends LslValue {
		public String val;

		public LslKey(String val) {
			this.val = val;
		}
		
		public String toString() {
			return val;
		}
	}
	
	public static class LslList extends LslValue {
		private List val;

		public LslList() {
		    val = new LinkedList();
		}
		
		public LslList(List val) {
			if (val == null) this.val = new LinkedList();
			else this.val = val;
		}
		
		public String toString() {
			StringBuilder buf = new StringBuilder("["); //$NON-NLS-1$
			String sep = BLANK;

			for (Iterator it = getVal().iterator(); it.hasNext(); ) {
				buf.append(sep).append(it.next().toString());
				sep = ","; //$NON-NLS-1$
			}
			
			return buf.append(']').toString();
		}

        public void setVal(List val) {
            if (val == null) val = new LinkedList();
            this.val = val;
        }

        public List getVal() {
            if (val == null) val = new LinkedList();
            return val;
        }
	}
	
	public static class LslVector extends LslValue {
		public float x,y,z;
		public LslVector(float x, float y, float z) {
			this.x = x;
			this.y = y;
			this.z = z;
		}
		
		public String toString() {
			return "<" + x + "," + y + "," + z + ">";  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		}
	}
	
	public static class LslRotation extends LslValue {
		public float x,y,z,s;
		public LslRotation(float x, float y, float z, float s) {
			this.x = x;
			this.y = y;
			this.z = z;
			this.s = s;
		}
		
		public String toString() {
			return "<" + x + "," + y + "," + z + "," + s + ">";  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
		}
	}

	public static class EntryPoint {
		private String fileName;
		private String path;
        public void setFileName(String fileName) {
            this.fileName = fileName;
        }
        public String getFileName() {
            return fileName;
        }
        public void setPath(String path) {
            this.path = path;
        }
        public String getPath() {
            return path;
        }
        
        public boolean equals(Object o) {
            if (o == null || !(o instanceof EntryPoint)) return false;
            EntryPoint other = (EntryPoint) o;
            return Util.safeEquals(fileName, other.fileName) &&
                   Util.safeEquals(path, other.path);
        }
	}
	
	public static class ExpectedCall {
		private String name;
		private LslValue returns;
		private List args = new ArrayList();
		
        public void setName(String funcName) {
            this.name = funcName;
        }
        public String getName() {
            return name;
        }
        public void setReturns(LslValue returns) {
            this.returns = returns;
        }
        public LslValue getReturns() {
            return returns;
        }
        public void setArgs(List args) {
            this.args = args;
        }
        public List getArgs() {
            if (args == null) args = new ArrayList();
            return args;
        }
	}
	
	public static class CallExpectations {
	    private static final String STRICT = "strict"; //$NON-NLS-1$
        private static final String EXHAUST = "exhaust"; //$NON-NLS-1$
        private static final String NORMAL = "normal"; //$NON-NLS-1$
        private static final String NICE = "nice"; //$NON-NLS-1$
        private String mode = NICE;
	    private ArrayList calls = new ArrayList();
	    private static TreeSet modeSet;
	    
	    static {
	        modeSet = new TreeSet();
	        modeSet.add(NICE);
	        modeSet.add(NORMAL);
	        modeSet.add(EXHAUST);
	        modeSet.add(STRICT);
	    }
	    
	    public static SortedSet getModes() {
	        return (SortedSet) modeSet.clone();
	    }
	    
	    public List getExpectedCalls() {
	        if (calls == null) calls = new ArrayList();
	        return calls;
	    }
	    
	    public String getMode() {
	        if (mode == null) mode = NICE;
	        return mode;
	    }
	    
	    public void setMode(String mode) {
	         if (!modeSet.contains(mode)) throw new RuntimeException(Messages.getString("LslTest.INVALID_MODE") + mode); //$NON-NLS-1$
	         this.mode = mode;
	    }
	    
	    public void addCall() {
	        calls.add(new ExpectedCall());
	    }
	    
	    public void postInit() {
	        if (mode == null) mode = NICE;
	        if (calls == null) calls = new ArrayList();
	    }
	}
	
	public static class GlobBinding {
	    private String name;
	    private LslValue value;
	    
	    public String getName() { return name; }
	    public LslValue getValue() { return value; }
        public void setValue(LslValue o) {
            value = o;
        }
        public void setName(String name) {
            this.name = name;
        }
	}
	
	public void postInit() {
	    if (this.arguments == null) this.arguments = new LslValue[0];
	    if (this.entryPoint == null) this.entryPoint = new EntryPoint();
	    if (this.expectedReturn == null) this.expectedReturn = new MaybeValue();
	    if (this.name == null) this.name = BLANK;
	    if (this.expectations == null) this.expectations = new CallExpectations();
	    if (this.initialBindings == null) this.initialBindings = new ArrayList();
	    this.expectations.postInit();
	}

	public String toString() {
		return "Test \"" + name + "\"";  //$NON-NLS-1$//$NON-NLS-2$
	}
	public static void main(String[] args) {
		XStream xstream = new XStream(new DomDriver());
		
		LslTest tst = new LslTest();
		tst.name = "Sample"; //$NON-NLS-1$
	    EntryPoint mod = new EntryPoint();
	    tst.entryPoint = mod;
		mod.setPath("sort"); //$NON-NLS-1$
		mod.setFileName("test.lslm"); //$NON-NLS-1$
		tst.expectedReturn = new MaybeValue();
		tst.expectedReturn.setVal(new LslInteger("2")); //$NON-NLS-1$
		System.out.println(xstream.toXML(tst));
	}

	public static LslValue defaultValueFor(Class argType) {
		if (LslString.class.equals(argType)) {
			return new LslString(EMPTY_STRING);
		} else if (LslKey.class.equals(argType)) {
			return new LslKey(EMPTY_STRING);
		} else if (LslInteger.class.equals(argType)) {
			return new LslInteger("0"); //$NON-NLS-1$
		} else if (LslFloat.class.equals(argType)) {
			return new LslFloat("0.0"); //$NON-NLS-1$
		} else if (LslList.class.equals(argType)) {
			return new LslList(null);
		} else if (LslVector.class.equals(argType)) {
			return new LslVector(0,0,0);
		} else if (LslRotation.class.equals(argType)) {
			return new LslRotation(0,0,0,1);
		} else return new LslVoid();
	}
	
    public void setExpectedReturn(MaybeValue expectedReturn) {
        this.expectedReturn = expectedReturn;
    }
    
    public MaybeValue getExpectedReturn() {
        return expectedReturn;
    }
    
    public CallExpectations getExpectations() {
        if (expectations == null) expectations = new CallExpectations();
        return expectations;
    }

    public void setExpectations(CallExpectations o) {
        this.expectations = o;
    }

    public ArrayList getInitialBindings() { return initialBindings; }
    public void setInitialBindings(ArrayList bindings) {
        this.initialBindings = bindings;
    }
    
    public static Class stringToLslType(String s) {
    	if ("integer".equals(s)) return LslInteger.class; //$NON-NLS-1$
    	else if ("float".equals(s)) return LslFloat.class; //$NON-NLS-1$
    	else if ("string".equals(s)) return LslString.class; //$NON-NLS-1$
    	else if ("key".equals(s)) return LslKey.class; //$NON-NLS-1$
    	else if ("vector".equals(s)) return LslVector.class; //$NON-NLS-1$
    	else if ("rotation".equals(s)) return LslRotation.class; //$NON-NLS-1$
    	else if ("list".equals(s)) return LslList.class; //$NON-NLS-1$
    	else if ("void".equals(s)) return LslVoid.class; //$NON-NLS-1$
    	else if (BLANK.equals(s)) return LslVoid.class;
    	return LslValue.class;
    }

    public void setFinalBindings(ArrayList finalBindings) {
        this.finalBindings = finalBindings;
    }

    public ArrayList getFinalBindings() {
        return finalBindings;
    }

    public void setEntryPoint(EntryPoint entryPoint) {
        this.entryPoint = entryPoint;
        
        this.expectations = new CallExpectations();
        this.expectations.postInit();
        this.expectedReturn = new MaybeValue();
        this.initialBindings = new ArrayList();
        this.finalBindings = new ArrayList();
        
        LslParam[] params = nature().getParams(entryPoint.getFileName(), entryPoint.getPath());
        
        this.arguments = new LslValue[params.length];
        
        for (int i = 0; i < params.length; i++) {
            this.arguments[i] = defaultValueFor(stringToLslType(params[i].getType()));
        }
    }

    public EntryPoint getEntryPoint() {
        return entryPoint;
    }

    public void setSuite(LslTestSuite lslTestSuite) {
        this.suite = lslTestSuite;
    }
    
    private LslProjectNature nature() {
        return suite.nature();
    }
}
