package lslplus.sim;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lslplus.util.ListConverter;
import lslplus.util.MapConverter;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;
import com.thoughtworks.xstream.mapper.Mapper;

/**
 *
 * Classes used for serializing setup info to haskell core.
 */
public class SimWorldDef {
    public static class Script {
        private String primKey;
        private String scriptName;
        private String scriptId;
        
        public Script(String primKey, String scriptName, String scriptId) {
            this.primKey = primKey;
            this.scriptName = scriptName;
            this.scriptId = scriptId;
        }
    }
    
    public static class ScriptInfo {
    	private String scriptName;
    	private String scriptId;
    	public ScriptInfo(String scriptName, String scriptId) {
    		this.scriptName = scriptName;
    		this.scriptId = scriptId;
    	}
    }
    
    public static abstract class InventoryItem {
        private String name;
        private String creator;
        public InventoryItem(String name, String creator) {
            this.name = name;
            this.creator = creator;
        }
    }
    
    public static class Notecard extends InventoryItem {
        private String[] lines;
        public Notecard(String name, String creator, String[] lines) {
            super(name,creator);
            this.lines = lines;
        }
    }
    
    public static class SimObject {
        private String[] primKeys;
        
        public SimObject(String[] primKeys) {
            this.primKeys = primKeys;
        }    
    }
    
    public static class Prim {
        private String name;
        private String key;
        private ScriptInfo[] scripts;
        private String description;
        private String owner;
        private LVector position;
        private LVector rotation;
        private Map data;
        
        public Prim(String name, String key, ScriptInfo[] scripts, Map data,
                String description, String owner, LVector position, LVector rotation) {
            this.name = name;
            this.key = key;
            this.scripts = scripts;
            this.data = data;
            this.description = description;
            this.owner = owner;
            this.position = position;
            this.rotation = rotation;
        }
    }
    
    public static class Avatar {
        private String key;
        private String name;
        private float xPos;
        private float yPos;
        private float zPos;
        
        public Avatar(String key, String name, float x, float y, float z) {
            this.key = key;
            this.name = name;
            this.xPos = x;
            this.yPos = y;
            this.zPos = z;
        }
    }
    
    public static class LVector {
        private float x,y,z;
        public LVector(float x, float y, float z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
        public float getX() { return x; }
        public float getY() { return y; }
        public float getZ() { return z; }
    }
    
    public static class LRotation {
        private float x,y,z,s;
        
        public LRotation(float x, float y, float z, float s) {
            this.x = x;
            this.y = y;
            this.z = z;
            this.s = s;
        }
    }
    
    private static XStream xstream = new XStream(new DomDriver());
    
    public static void configureXStream(XStream xstream) {
        xstream.setMode(XStream.NO_REFERENCES);
        xstream.alias("world-def", SimWorldDef.class); //$NON-NLS-1$
        xstream.alias("avatar", Avatar.class); //$NON-NLS-1$
        xstream.alias("script", ScriptInfo.class); //$NON-NLS-1$
        xstream.alias("object", SimObject.class); //$NON-NLS-1$
        xstream.alias("prim", Prim.class); //$NON-NLS-1$
        Mapper mapper = xstream.getMapper();
        xstream.registerConverter(new MapConverter(mapper));
        xstream.registerConverter(new ListConverter(mapper));
        xstream.alias("list", List.class); //$NON-NLS-1$
        xstream.alias("map", Map.class); //$NON-NLS-1$
        xstream.alias("vector", LVector.class); //$NON-NLS-1$
        xstream.alias("rotation", LRotation.class); //$NON-NLS-1$
    }
    
    static {
        configureXStream(xstream);
    }
    
    public static String toXML(SimWorldDef def) {
        return xstream.toXML(def);
    }
    
    private long maxTime;
    private int sliceSize;
    //private Script[] scripts;
    private SimObject[] objects;
    private Prim[] prims;
    private Avatar[] avatars;
    
    public SimWorldDef(long maxTime, int sliceSize, SimObject[] objects,
            Prim[] prims, Avatar[] avatars) {
        this.maxTime = maxTime;
        this.sliceSize = sliceSize;
        //this.scripts = scripts;
        this.objects = objects;
        this.prims = prims;
        this.avatars = avatars;
    }

    public static SimWorldDef mkSimpleWorld(SimKeyManager keyManager, String name) {
        String primKey = keyManager.getNextKey();
//        SimWorldDef.Script[] scripts = new SimWorldDef.Script[] {
//                new SimWorldDef.Script(primKey, name, name)
//        };
        String avKey = keyManager.getNextKey();
        
        HashMap primData = new HashMap();
        primData.put("pos", new LVector(128,128,0)); //$NON-NLS-1$
        SimWorldDef.Prim[] prims = new SimWorldDef.Prim[] {
                new SimWorldDef.Prim("defaultPrim", primKey, new ScriptInfo[] { new ScriptInfo(name,name) }, primData,
                        "an object", avKey, new LVector(128,128,0), new LVector(0,0,0))
        };
        
        SimWorldDef.SimObject[] objects = new SimWorldDef.SimObject[] {
                new SimWorldDef.SimObject(new String[] { primKey })
        };
        
        SimWorldDef.Avatar[] avatars = new SimWorldDef.Avatar[] {
                new SimWorldDef.Avatar(avKey, "Default Avatar", 128, 128, 0)
        };
        
        SimWorldDef def = new SimWorldDef(10000000,1000,objects,prims,avatars);
        
        return def;
    }
    
    
}
