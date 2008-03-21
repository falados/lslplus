package lslplus.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Array;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import lslplus.LslPlusPlugin;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;

public class Util {
   
    /**
     * Find the absolute offset in a file of a give line/column.  Lines start at 0, and
     * columns include the notion of a tab stop, where there is a tab stop every 8 columns. So
     * the 4th character on a line could be at column 24, if the first 3 characters are tabs.  If
     * the first two characters are non-tabs, and the third character is a tab, then the 4th
     * character is at column 8.  But if the first character is a tab, the next two are non tabs,
     * then the 4th character is at column 10.  Etc.  This convoluted algorithm is necessary because
     * Eclipse annotations are based on absolute start and end character offsets in a file, but
     * the parser currently used for parsing LSL has a notion of lines and columns, which includes
     * tab stops in its column calculations.  As a future upgrade, one might envision hacking the
     * underlying parser library (Haskell's Parsec) to include absolute file position in addition
     * to line/column (because the whole line column thing, in addition to requiring the convoluted
     * code below, also means we have to reread in Java every file that we originally read in
     * in the compiler.  Which is unfortunate.).
     * @param lines the line offset
     * @param columns the column offset
     * @param f the file
     * @return
     */
    public static int[] findOffsetsFor(int[] lines, int[] columns, IFile f) {
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new InputStreamReader(f.getContents()));
            
            int index = 0;
            int lineIndex = 0;
            int colIndex = 0;
            int[] result = new int[lines.length];
            for (int i = 0; i < lines.length; i++) {
                while (lineIndex < lines[i]) {
                    int ch = reader.read();
                    if (ch < 0) break;
                    if (ch == '\n') {
                        lineIndex++;
                        colIndex = 0;
                    }
                    index++;
                }
                
                boolean incIndex = false;
                while (colIndex < columns[i]) {
                    int ch = reader.read();
                    if (ch < 0) break;
                    if (ch == '\n') {
                        lineIndex++;
                        incIndex = true;
                        colIndex = 0;
                        break;
                    }
                    if (ch == '\t') colIndex += (8 - colIndex % 8);
                    else colIndex += 1;
                    index++;
                }
                result[i] = index;
                if (incIndex) index++;
            }
            return result;
        } catch (Exception e) {
            Util.log(e, e.getLocalizedMessage());
            return null;
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException e) {
                }
            }
        }
        
    }
    public static List compulteLineOffsets(IFile f) {
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(f.getContents()));
            
            ArrayList list = new ArrayList();
            
            list.add(new Integer(0));
            int ch;
            int index = 0;
            while ((ch = reader.read()) >= 0) {
                index++;
                if (ch == '\n') {
                    list.add(new Integer(index));
                }
            }
            list.add(new Integer(index));
            
            return list;
        } catch (Exception e) {
            Util.log(e, e.getLocalizedMessage());
            return null;
        }
    }
	/*
	 * Add a log entry
	 */
	public static void log(Throwable e, String message) {
		IStatus status= new Status(
			IStatus.ERROR, 
			"lslplus",  //$NON-NLS-1$
			IStatus.ERROR, 
			message, 
			e); 
		LslPlusPlugin.getDefault().getLog().log(status);
	}

	public static void log(String message) {
		IStatus status= new Status(
			IStatus.INFO, 
			"lslplus",  //$NON-NLS-1$
			IStatus.INFO, 
			message, 
			null); 
		LslPlusPlugin.getDefault().getLog().log(status);
	}

	public static Object[] append(Object[] lst,Object[] lst1) {
		Object[] l = (Object[]) Array.newInstance(lst.getClass().getComponentType(), lst.length + lst1.length);
		System.arraycopy(lst, 0, l, 0, lst.length);
		System.arraycopy(lst1, 0, l, lst.length, lst1.length);
		return l;
	}
	
	public static Object[] concat(Object[][] lsts) {
		int totLength = 0;
		Class componentType = null;
		
		for (int i = 0; i < lsts.length; i++) {
			Object[] lst = lsts[i];
			if (lst == null) continue;
			if (componentType == null) componentType = lst.getClass().getComponentType();
			else {
				Class newComponentType = lst.getClass().getComponentType();
				
				if (newComponentType != componentType) {
					if (!componentType.isAssignableFrom(newComponentType)) {
						if (newComponentType.isAssignableFrom(componentType)) {
							componentType = newComponentType;
						} else {
							// punt!
							componentType = Object.class;
						}
					}
				}
			}
			
			totLength += lst.length;
		}
		
		if (componentType == null) {
			return new Object[0];
		}
		Object[] l = (Object[]) Array.newInstance(componentType, totLength);
		int offset = 0;
		for (int i = 0; i < lsts.length; i++) {
			Object[] lst = lsts[i];
			if (lst == null) continue;
			
			System.arraycopy(lst, 0, l, offset, lst.length);
			offset += lst.length;
		}
		
		return l;
	}
	
	public static void error(String message) {
		IStatus status= new Status(
				IStatus.INFO, 
				"lslplus",  //$NON-NLS-1$
				IStatus.INFO, 
				message, 
				null); 
			LslPlusPlugin.getDefault().getLog().log(status);
	}
	
	public static interface ArrayMapFunc {
		public Object map(Object o);
		public Class elementType();
	}
	
	public static interface Predicate {
		public boolean test(Object o);
	}
	
	public static Object find(Predicate p, Object[] list) {
		for (int i = 0; i < list.length; i++) {
			if (p.test(list[i])) return list[i];
		}
		return null;
	}
	
	public static Object[] arrayMap(ArrayMapFunc f, Object[] list) {
		Object[] o = (Object[]) Array.newInstance(f.elementType(), list.length);
		
		for (int i = 0; i < list.length; i++) {
			o[i] = f.map(list[i]);
		}
		
		return o;
	}
	
	public static List filtMap(ArrayMapFunc f, Object[] list) {
	    LinkedList result = new LinkedList();
	    
	    for (int i = 0; i < list.length; i++) {
	        Object o = f.map(list[i]);
	        
	        if (o != null) result.add(o);
	    }
	    
	    return result;
	}
	public static ImageDescriptor findDescriptor(String spath) {
		IPath path = new Path(spath);
		URL url = 
			FileLocator.find(LslPlusPlugin.getDefault().getBundle(), path, null);
		if (url != null) {
			return ImageDescriptor.createFromURL(url);
		} else {
		    return ImageDescriptor.getMissingImageDescriptor();
		}

	}
	
	public static String quote(String s) {
		return new StringBuilder("\"").append(s).append('"').toString(); //$NON-NLS-1$
	}

    public static int elementIndex(Object o, Object[] os) {
        if (o == null || os == null) return -1;
        
        for (int i = 0; i < os.length; i++) {
            if (o.equals(os[i])) return i;
        }
        return -1;
    }

    public static Set mapToSet(ArrayMapFunc arrayMapFunc, List globs) {
        HashSet set = new HashSet();
        for (Iterator i = globs.iterator(); i.hasNext(); ) {
            set.add(arrayMapFunc.map(i.next()));
        }
        
        return set;
    }
    
    public static boolean safeEquals(Object o0, Object o1) {
        return o0 == o1 || (o0 != null && o0.equals(o1));
    }
}
