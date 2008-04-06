package lslplus.simview;

import java.util.Iterator;
import java.util.LinkedList;

import lslplus.LslPlusPlugin;
import lslplus.sim.SimStatuses;

import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;

public class LogViewerLabelProvider implements ITableLabelProvider {
    
    private LinkedList listeners = new LinkedList();
    private LinkedList images = new LinkedList();
    private Image infoImg = createImage("icons/info_st_obj.gif");
    
    public Image getColumnImage(Object element, int columnIndex) {
        if (element instanceof SimStatuses.Message) {
            SimStatuses.Message message = (SimStatuses.Message) element;
            if (columnIndex == 2) {
                if (SimStatuses.Message.INFO_LEVEL.equals(message.getLevel())) {
                    return infoImg;
                }
            }
        }
        return null;
    }

    private Image createImage(String path) {
        // TODO Auto-generated method stub
        Image image = LslPlusPlugin.createImage(path);
        images.add(image);
        return image;
    }

    public String getColumnText(Object element, int columnIndex) {
        if (element instanceof SimStatuses.Message) {
            SimStatuses.Message message = (SimStatuses.Message) element;
            if (columnIndex == 0) return message.getTime();
            else if (columnIndex == 1) return message.getSource();
            else return message.getText();
        } else {
            return "";
        }
    }

    public void addListener(ILabelProviderListener listener) {
        listeners.add(listener);
    }

    public void dispose() {
        listeners.clear();
        
        for (Iterator i = images.iterator(); i.hasNext();) {
            Image img = (Image)i.next();
            img.dispose();
        }
        
        images.clear();
    }

    public boolean isLabelProperty(Object element, String property) {
        return false;
    }

    public void removeListener(ILabelProviderListener listener) {
        listeners.remove(listener);
    }
}
