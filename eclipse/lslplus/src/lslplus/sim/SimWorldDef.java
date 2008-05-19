package lslplus.sim;

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
    
    public static class Texture extends InventoryItem {
        public Texture(String name, String creator) {
            super(name, creator);
        }
    }
    
    public static class Clothing extends InventoryItem {
        public Clothing(String name, String creator) {
            super(name, creator);
        }
    }
    
    public static class BodyPart extends InventoryItem {
        public BodyPart(String name, String creator) {
            super(name, creator);
        }
    }
    
    public static class Gesture extends InventoryItem {
        public Gesture(String name, String creator) {
            super(name, creator);
        }
    }
    
    public static class Animation extends InventoryItem {
        private float duration;

        public Animation(String name, String creator, float duration) {
            super(name, creator);
            this.duration = duration;
        }
    }
    public static class Sound extends InventoryItem {
        private float duration;

        public Sound(String name, String creator, float duration) {
            super(name, creator);
            this.duration = duration;
        }
    }
    
    public static class Landmark extends InventoryItem {
        private Region region;
        private LVector position;
        public Landmark(String name, String creator, Region region, LVector position) {
            super(name,creator);
            this.position = position;
            this.region = region;
        }
    }
    
    public static class InventoryObject extends InventoryItem {
        private Prim[] prims;
        public InventoryObject(String name, String creator, Prim[] prims) {
            super(name,creator);
            this.prims = prims;
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
        private InventoryItem[] inventory;
        private String description;
        private String owner;
        private LVector position;
        private LVector rotation;
        
        public Prim(String name, String key, ScriptInfo[] scripts, InventoryItem[] inventory,
                String description, String owner, LVector position, LVector rotation) {
            this.name = name;
            this.key = key;
            this.scripts = scripts;
            this.inventory = inventory;
            this.description = description;
            this.owner = owner;
            this.position = position;
            this.rotation = rotation;
        }

        public String getKey() {
            return key;
        }
    }
    
    public static class Avatar {
        private String name;
        private float xPos;
        private float yPos;
        private float zPos;
        
        public Avatar(String name, float x, float y, float z) {
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
    
    public static class Region {
        private int x,y;
        public Region(int x, int y) { this.x = x; this.y = y; }
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
        xstream.alias("region", Region.class);
        xstream.alias("inventoryItem", InventoryItem.class); //$NON-NLS-1$
        xstream.aliasType("notecardItem", Notecard.class); //$NON-NLS-1$
        xstream.aliasType("bodyPartItem", BodyPart.class);
        xstream.aliasType("textureItem", Texture.class);
        xstream.aliasType("animationItem", Animation.class);
        xstream.aliasType("soundItem", Sound.class);
        xstream.aliasType("gestureItem", Gesture.class);
        xstream.aliasType("clothingItem", Clothing.class);
        xstream.aliasType("landmarkItem", Landmark.class);
        xstream.aliasType("inventoryObjectItem", InventoryObject.class);
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
    private String simEventHandler;
    
    public SimWorldDef(long maxTime, int sliceSize, SimObject[] objects,
            Prim[] prims, Avatar[] avatars, String simEventHandler) {
        this.maxTime = maxTime;
        this.sliceSize = sliceSize;
        this.objects = objects;
        this.prims = prims;
        this.avatars = avatars;
        this.simEventHandler = simEventHandler;
    }

    public static SimWorldDef mkSimpleWorld(SimKeyManager keyManager, String name) {
        String primKey = keyManager.getNextKey();
        String avKey = keyManager.getNextKey();
        
        SimWorldDef.Prim[] prims = new SimWorldDef.Prim[] {
                new SimWorldDef.Prim("defaultPrim", primKey, new ScriptInfo[] { new ScriptInfo(name,name) }, null,
                        "an object", "Default Avatar", new LVector(128,128,0), new LVector(0,0,0))
        };
        
        SimWorldDef.SimObject[] objects = new SimWorldDef.SimObject[] {
                new SimWorldDef.SimObject(new String[] { primKey })
        };
        
        SimWorldDef.Avatar[] avatars = new SimWorldDef.Avatar[] {
                new SimWorldDef.Avatar("Default Avatar", 128, 128, 0)
        };
        
        SimWorldDef def = new SimWorldDef(10000000,1000,objects,prims,avatars, null);
        
        return def;
    }
    
    
}
