package lslplus.editor;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;

import lslplus.LslPlusPlugin;
import lslplus.editor.LslTestContentProvider.ArgumentsNode;
import lslplus.editor.LslTestContentProvider.CallArgs;
import lslplus.editor.LslTestContentProvider.CallListNode;
import lslplus.editor.LslTestContentProvider.CallNode;
import lslplus.editor.LslTestContentProvider.EntryPointNode;
import lslplus.editor.LslTestContentProvider.ExpectationsModeNode;
import lslplus.editor.LslTestContentProvider.ExpectationsNode;
import lslplus.editor.LslTestContentProvider.GlobalBindingNode;
import lslplus.editor.LslTestContentProvider.GlobalBindingsNode;
import lslplus.editor.LslTestContentProvider.LslValueNode;
import lslplus.editor.LslTestContentProvider.MaybeNode;
import lslplus.editor.LslTestContentProvider.Node;
import lslplus.editor.LslTestContentProvider.TestNode;
import lslplus.editor.LslTestContentProvider.CallNode.CallNameNode;
import lslplus.util.Util;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;


public class LslTestLabelProvider
	extends LabelProvider
	implements ITableLabelProvider {
	
	private static final String ICONS_OBJ16_LSLPLUS_TEST_GIF = "icons/obj16/lslplus-test.gif"; //$NON-NLS-1$

	private LinkedList images;
	private Image testImage = createImage("icons/test.gif"); //$NON-NLS-1$
    private Image suiteImage = createImage("icons/tsuite.gif"); //$NON-NLS-1$
    private Image returnsImage = createImage("icons/returns.gif"); //$NON-NLS-1$
    private Image argsImage = createImage("icons/args1.gif"); //$NON-NLS-1$
    private Image functionImage = createImage("icons/function.gif"); //$NON-NLS-1$;
    private Image callImage = createImage("icons/call.gif"); //$NON-NLS-1$;
    private Image callListImage = createImage("icons/calls.gif"); //$NON-NLS-1$;
    private Image expectationsImage = createImage("icons/call-expectations.gif"); //$NON-NLS-1$;
    private Image argImage = createImage("icons/arg.gif"); //$NON-NLS-1$
    private Image modeImage = createImage("icons/mode.gif"); //$NON-NLS-1$
    private Image entryPointImage = createImage("icons/entry-point.gif"); //$NON-NLS-1$
    private Image initialGlobalsImage = createImage("icons/initial-globs.gif"); //$NON-NLS-1$
    private Image finalGlobalsImage = createImage("icons/final-globs.gif"); //$NON-NLS-1$
    private Image initialGlobalImage = createImage("icons/initial-glob.gif"); //$NON-NLS-1$
    private Image finalGlobalImage = createImage("icons/final-glob.gif"); //$NON-NLS-1$
	private ArrayList consumers = new ArrayList();


	public LslTestLabelProvider() {
	}
	
	private Image createImage(String path) {
	    if (images == null) images = new LinkedList();
	    Image i = LslPlusPlugin.createImage(path);
	    images.add(i);
	    return i;
	}
	
	public static Image dummy() {
		return Util.findDescriptor(ICONS_OBJ16_LSLPLUS_TEST_GIF).createImage();
	}
	
	public void dispose() {
		if (consumers.size() == 0){
		    for (Iterator i = images.iterator(); i.hasNext(); ) {
		        ((Image)i.next()).dispose();
		    }
			super.dispose();
		}
	}
	
	public Image getColumnImage(Object element, int columnIndex) {
		if (columnIndex == 0) {
		    if (element instanceof LslTestContentProvider.SuiteNode) {
		        return suiteImage;
		    } else if (element instanceof LslTestContentProvider.TestNode) {
		        return testImage;
		    } else if (element instanceof LslTestContentProvider.LslValueNode) {
		        LslValueNode node = (LslValueNode) element;
		        if (node.getParent() instanceof TestNode ||
		            node.getParent() instanceof CallNode) {
		            return returnsImage;
		        } else {
		            return argImage;
		        }
		    } else if (element instanceof MaybeNode) {
		        Node n = (Node) element;
		        if (n.getParent() instanceof CallArgs) {
		            return argImage;
		        } else {
		            return returnsImage;
		        }
		    } else if (element instanceof CallListNode) {
		        return callListImage;
		    } else if (element instanceof ArgumentsNode) {
		        return argsImage;
		    } else if (element instanceof CallNameNode) {
		        return functionImage;
		    } else if (element instanceof CallNode) {
		        return callImage;
		    } else if (element instanceof CallArgs) {
		        return argsImage;
		    } else if (element instanceof ExpectationsNode) {
		        return expectationsImage;
		    } else if (element instanceof ExpectationsModeNode) {
		        return modeImage;
		    } else if (element instanceof EntryPointNode) {
		        return entryPointImage;
		    } else if (element instanceof GlobalBindingNode) {
		        GlobalBindingsNode node = (GlobalBindingsNode) ((Node)element).getParent();
		        return node.isInitial() ? initialGlobalImage : finalGlobalImage;
		    } else if (element instanceof GlobalBindingsNode) {
		        GlobalBindingsNode node = (GlobalBindingsNode) element;
		        return node.isInitial() ? initialGlobalsImage : finalGlobalsImage;
		    }
		    return dummy();
		}
		return null;
	}
	
	public String getColumnText(Object element, int columnIndex) {
		Node n = (Node) element;
		
		if (columnIndex == 0) return n.getName();
		else if (columnIndex == 1) {
			return n.displayString();
		} else return null;
	}

	public void connect(Object consumer) {
		if (!consumers.contains(consumer))
			consumers.add(consumer);
	}
	
	public void disconnect(Object consumer) {
		consumers.remove(consumer);
		if (consumers.size() == 0) {
			dispose();
		}
	}
}
