package lslplus.editor;

import java.util.Iterator;
import java.util.LinkedList;

import lslplus.LslPlusPlugin;
import lslplus.sim.SimProject;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

public class SimProjectLabelProvider extends LabelProvider implements ITableLabelProvider {
    private LinkedList images;
    private Image objectImage = createImage("icons/object.gif"); //$NON-NLS-1$
    private Image primImage = createImage("icons/prim.gif"); //$NON-NLS-1$
    private Image avatarImage = createImage("icons/avatar.gif"); //$NON-NLS-1$
    private Image avatarRefImage = createImage("icons/avatar-ref.gif"); //$NON-NLS-1$
    private Image scriptImage = createImage("icons/obj16/lslplus.gif"); //$NON-NLS-1$
    private Image valImage = createImage("icons/valimage.gif"); //$NON-NLS-1$
    private Image worldImage = createImage("icons/world.gif"); //$NON-NLS-1$
    private Image propertiesImage = createImage("icons/properties.gif"); //$NON-NLS-1$
    private Image gridPositionImage = createImage("icons/grid-position.gif"); //$NON-NLS-1$
    private Image xPositionImage = createImage("icons/x-position.gif"); //$NON-NLS-1$
    private Image yPositionImage = createImage("icons/y-position.gif"); //$NON-NLS-1$
    private Image zPositionImage = createImage("icons/z-position.gif"); //$NON-NLS-1$
    private Image createImage(String path) {
        if (images == null) images = new LinkedList();
        Image i = LslPlusPlugin.createImage(path);
        if (i != null) images.add(i);
        return i;
    }
    

    public Image getColumnImage(Object element, int columnIndex) {
        if (columnIndex > 0) return null;
        if (element instanceof SimProject.ObjectNode) {
            return objectImage;
        } else if (element instanceof SimProject.PrimNode) {
            return primImage;
        } else if (element instanceof SimProject.AvatarNode) {
            return avatarImage;
        } else if (element instanceof SimProject.ScriptNode) {
            return scriptImage;
        } else if (element instanceof SimProject.WorldNode) {
            return worldImage;
        } else if (element instanceof SimProject.PrimPropertiesNode ||
                   element instanceof SimProject.AvatarPropertiesNode) {
            return propertiesImage;
        } else if (element instanceof SimProject.GridPositionNode) {
            return gridPositionImage;
        } else if (element instanceof SimProject.GridCoordinateNode) {
            SimProject.GridCoordinateNode n = (SimProject.GridCoordinateNode) element;
            if (n.getName().startsWith("x")) return xPositionImage;
            if (n.getName().startsWith("y")) return yPositionImage;
            if (n.getName().startsWith("z")) return zPositionImage;
            return valImage;
        } else if (element instanceof SimProject.AvatarReferenceNode) {
            return avatarRefImage;
        } else {
            // TODO: add other node types
            return valImage;
        }
    }

    public String getColumnText(Object element, int columnIndex) {
        if (columnIndex == 0) return ((SimProject.Node)element).getNameDisplay();
        else if (columnIndex == 1) return ((SimProject.Node)element).getValue().toString();
        return null;
    }

    public void dispose() {
        super.dispose();
        for (Iterator i = images.iterator(); i.hasNext(); ) {
            Image img = (Image) i.next();
            img.dispose();
        }
        
        images.clear();
    }
}
