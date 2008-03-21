package lslplus.lsltest;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;

import lslplus.LslProjectNature;
import lslplus.lsltest.LslTest.ExpectedCall;
import lslplus.lsltest.LslTest.GlobBinding;
import lslplus.lsltest.LslTest.LslFloat;
import lslplus.lsltest.LslTest.LslInteger;
import lslplus.lsltest.LslTest.LslKey;
import lslplus.lsltest.LslTest.LslList;
import lslplus.lsltest.LslTest.LslRotation;
import lslplus.lsltest.LslTest.LslString;
import lslplus.lsltest.LslTest.LslValue;
import lslplus.lsltest.LslTest.LslVector;
import lslplus.lsltest.LslTest.LslVoid;
import lslplus.lsltest.LslTest.MaybeValue;
import lslplus.util.Util;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.Platform;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.converters.SingleValueConverter;
import com.thoughtworks.xstream.converters.basic.FloatConverter;
import com.thoughtworks.xstream.converters.basic.IntConverter;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class LslTestSuite implements IAdaptable {
	private static class XMLSerializer {
		private static XStream xstream;
		
		public static LslTestSuite fromXML(String xml) {
			return (LslTestSuite) xstream.fromXML(xml);
		}
		
		public static String toXML(LslTestSuite suite) {
			return xstream.toXML(suite);
		}
		
		private static XStream xstreamExternal;
	}
	
	static {
		XStream xstream = createXStream();
        
        XMLSerializer.xstream = xstream;
        XStream xstreamExternal = createXStream();
        xstreamExternal.omitField(LslValue.class, "mnemonic"); //$NON-NLS-1$
        XMLSerializer.xstreamExternal = xstreamExternal;
	}

    private static XStream createXStream() {
        XStream xstream = new XStream(new DomDriver());
		xstream.omitField(LslTest.class, "suite"); //$NON-NLS-1$
		xstream.alias("tests", LslTestSuite.class); //$NON-NLS-1$
		xstream.omitField(LslTestSuite.class, "resource"); //$NON-NLS-1$
		xstream.aliasType("lsl-integer", LslInteger.class); //$NON-NLS-1$
		xstream.aliasType("lsl-string", LslString.class); //$NON-NLS-1$
		xstream.aliasType("lsl-key", LslKey.class); //$NON-NLS-1$
		xstream.aliasType("lsl-float", LslFloat.class); //$NON-NLS-1$
		xstream.aliasType("lsl-vector", LslVector.class); //$NON-NLS-1$
		xstream.aliasType("lsl-rotation", LslRotation.class); //$NON-NLS-1$
		xstream.aliasType("lsl-list", LslList.class); //$NON-NLS-1$
		xstream.aliasType("lsl-void", LslVoid.class); //$NON-NLS-1$
		xstream.alias("lsl-test", LslTest.class); //$NON-NLS-1$
		xstream.alias("globalBinding", GlobBinding.class); //$NON-NLS-1$
		xstream.addImplicitCollection(LslTestSuite.class, "tests", LslTest.class); //$NON-NLS-1$
		xstream.addImplicitCollection(LslList.class, "val"); //$NON-NLS-1$
		xstream.alias("maybe-value", MaybeValue.class); //$NON-NLS-1$
		xstream.omitField(MaybeValue.class, "type"); //$NON-NLS-1$
		xstream.alias("call", ExpectedCall.class); //$NON-NLS-1$
		xstream.registerConverter(new SingleValueConverter() {
		    private FloatConverter conv = new FloatConverter();
            public Object fromString(String arg0) {
                
                return new LslFloat(arg0);
            }

            public String toString(Object arg0) {
                return conv.toString(((LslFloat)arg0).val);
            }

            public boolean canConvert(Class arg0) {
                return LslFloat.class.equals(arg0);
            }
		    
		});
        xstream.registerConverter(new SingleValueConverter() {
            private IntConverter conv = new IntConverter();
            public Object fromString(String arg0) {
                
                return new LslInteger(arg0);
            }

            public String toString(Object arg0) {
                return conv.toString(((LslInteger)arg0).val);
            }

            public boolean canConvert(Class arg0) {
                return LslInteger.class.equals(arg0);
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            public Object fromString(String arg0) {
                
                return new LslString(arg0);
            }

            public String toString(Object arg0) {
                return ((LslString)arg0).val;
            }

            public boolean canConvert(Class arg0) {
                return LslString.class.equals(arg0);
            }
            
        });
        xstream.registerConverter(new SingleValueConverter() {
            public Object fromString(String arg0) {
                
                return new LslKey(arg0);
            }

            public String toString(Object arg0) {
                return ((LslKey)arg0).val;
            }

            public boolean canConvert(Class arg0) {
                return LslKey.class.equals(arg0);
            }
            
        });
        return xstream;
    }
	
	private IResource resource;

	private ArrayList tests;
	
	public static LslTestSuite empty() {
		return new LslTestSuite();
	}
	
	public LslTestSuite() {
		this.tests = new ArrayList();
	}
	
	public void setIResource(IResource resource) {
		this.resource = resource;
	}
	
	public void addTest(LslTest test) {
	    test.setSuite(this);
		this.tests.add(test);
	}
	
	public void removeTest(int index) {
	    tests.remove(index);
	}
	
	public String toXml() {
		return XMLSerializer.toXML(this);
	}
	
	public String toExternalXml() {
	    return XMLSerializer.xstreamExternal.toXML(this);
	}
	
	public static LslTestSuite fromXml(String xml) {
		return XMLSerializer.fromXML(xml).postInit();
	}
	
	private LslTestSuite postInit() {
	    if (tests == null) tests = new ArrayList();
	    for (Iterator i = tests.iterator(); i.hasNext(); ) {
	        LslTest test = (LslTest) i.next();
	        test.setSuite(this);
	        test.postInit();
	    }
	    
	    return this;
	}
	public static LslTestSuite fromXml(InputStream input, IResource resource) {
		LslTestSuite suite = ((LslTestSuite) XMLSerializer.xstream.fromXML(input)).postInit();
		suite.setIResource(resource);
		return suite;
	}
	
	public static void main(String args[]) {
	    LslTestSuite suite = new LslTestSuite();
	    LslTest test = new LslTest();
	    LinkedList list = new LinkedList();
	    list.add(new LslFloat("1.0")); //$NON-NLS-1$
	    test.arguments = new LslValue[] { new LslList(list) };
	    test.setExpectedReturn(new MaybeValue()); //new MaybeValue(new LslVector(1.0f,2.0f,3.0f));
	    suite.addTest(test);
		String xml = suite.toXml();
		System.out.println(xml);
		LslTestSuite s = fromXml(xml);
		System.out.println("ok"); //$NON-NLS-1$
		xml = s.toXml();
		System.out.println(xml);
	}

	public LslTest[] getTests() {
	    if (tests == null) tests = new ArrayList();
		return (LslTest[]) tests.toArray(new LslTest[tests.size()]);
	}

	public Object getAdapter(Class adapter) {
        return Platform.getAdapterManager().getAdapter(this, adapter);
    }

	public IResource getResource() {
		return resource;
	}

    public void removeTest(LslTest value) {
        this.tests.remove(value);
    }

    public LslProjectNature nature() {
        try {
            return (LslProjectNature) resource.getProject().getNature(LslProjectNature.ID);
        } catch (CoreException e) {
            Util.log(e, e.getLocalizedMessage());
            return null;
        }
    }
	
}
