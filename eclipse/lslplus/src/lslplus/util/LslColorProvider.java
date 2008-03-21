package lslplus.util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

/**
 * Manager for colors used in the LSL Plus Editor.  This provider creates colors lazily as needed
 * and then caches them.
 */
public class LslColorProvider {

    public static final RGB DEFAULT = new RGB(0, 0, 0);
    public static final RGB HANDLER = new RGB(0, 0, 128);
    public static final RGB KEYWORD = new RGB(100, 25, 50);
    public static final RGB MULTI_LINE_COMMENT = new RGB(64, 64, 128);
    public static final RGB PREDEF_CONST = new RGB(0, 128, 0);
    public static final RGB PREDEF_FUNC = new RGB(128, 0, 0);
    public static final RGB SINGLE_LINE_COMMENT = new RGB(64, 64, 128);
    public static final RGB STRING = new RGB(0, 128, 128);
    public static final RGB TYPE = new RGB(0, 0, 128);

    protected Map colorTable = new HashMap(10);

    /**
     * Release all of the color resources held onto by the receiver.
     */
    public void dispose() {
        Iterator e = colorTable.values().iterator();
        while (e.hasNext())
            ((Color) e.next()).dispose();
    }

    /**
     * Return the color that is stored in the color table under the given RGB
     * value.
     * 
     * @param rgb the RGB value
     * @return the color stored in the color table for the given RGB value
     */
    public Color getColor(RGB rgb) {
        Color color = (Color) colorTable.get(rgb);
        if (color == null) {
            color = new Color(Display.getCurrent(), rgb);
            colorTable.put(rgb, color);
        }
        return color;
    }
}
