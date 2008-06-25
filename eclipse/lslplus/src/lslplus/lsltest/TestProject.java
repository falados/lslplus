package lslplus.lsltest;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import lslplus.LslExpressionValidator;
import lslplus.LslPlusPlugin;
import lslplus.LslProjectNature;
import lslplus.LslProjectNature.NameTypePair;
import lslplus.gentree.Node;
import lslplus.gentree.NodeFactory;
import lslplus.gentree.NodeFactory2;
import lslplus.gentree.NodeStatus;
import lslplus.language_metadata.LslFunction;
import lslplus.language_metadata.LslParam;
import lslplus.lsltest.LslTest.CallExpectations;
import lslplus.lsltest.LslTest.EntryPoint;
import lslplus.lsltest.LslTest.ExpectedCall;
import lslplus.lsltest.LslTest.GlobBinding;
import lslplus.lsltest.LslTest.LslValue;
import lslplus.lsltest.LslTest.MaybeValue;
import lslplus.util.Util;
import lslplus.util.Util.Predicate;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.DomDriver;

public class TestProject {
    private static final NodeFactory[] EMPTY_FACTORY_LIST = {};
    public static class SuiteNode extends Node {
        private transient IResource resource;
        private static final NodeFactory[] LEGAL_CHILDREN = { TEST_NODE_FACTORY };
        public SuiteNode(Node parent, String testSuiteName) {
            super(null, testSuiteName, null);
        }

        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return null;
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return true;
        }

        public boolean isValueChangeable() {
            return false;
        }

        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILDREN;
        }

        protected void onUpdate(String s) {
        }
        
        public LslProjectNature nature() {
            try {
                return (LslProjectNature) resource.getProject().getNature(LslProjectNature.ID);
            } catch (CoreException e) {
                Util.log(e, e.getLocalizedMessage());
                return null;
            }
        }

        public void setResource(IFile file) {
            this.resource = file;
        }
    }
    
    private static final NodeFactory TEST_NODE_FACTORY = new NodeFactory2() {
        public Node createNode(Node parent) {
            throw new UnsupportedOperationException();
        }

        public String getNodeTypeName() {
            return "Test";
        }

        public Node createNode(Node parent, String value) {
            return new TestNode(parent,"New  Test", value);
        }

        public String getNodeCreationId() {
            return "entry-point";
        }
    };
    

    public static class TestNode extends Node {

        public TestNode(Node parent, String testName, String value) {
            super(parent, testName, value);
            String[] elements = value.toString().split("/");
            Node root = findRoot();
            LslParam[] params = ((SuiteNode)root).nature().getParams(elements[0], elements[1]);
            String returnType = ((SuiteNode)root).nature().getReturnType(elements[0], elements[1]);
            addChild(new ArgumentsListNode(this, "args", params));
            addChild(new ExpectedReturnNode(this,returnType,null));
            addChild(new BindingListNode(this,"initial", "Initial Globals"));
            addChild(new ExpectationsNode(this));
            addChild(new BindingListNode(this,"final", "Final Globals"));
        }

        public ExpectationsNode getExpectations() {
            return (ExpectationsNode) findChildByName("expectations");
        }
        
        public ArgumentsListNode getArguments() {
            return (ArgumentsListNode) findChildByName("args");
        }
        
        public ExpectedReturnNode getReturn() {
            return (ExpectedReturnNode) findChildByName("returns");
        }
        
        public BindingListNode getInitialBindings() {
            return (BindingListNode) findChildByName("initial");
        }

        public BindingListNode getFinalBindings() {
            return (BindingListNode) findChildByName("final");
        }
        
        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return getValue().toString();
        }

        public boolean isDeletable() {
            return true;
        }

        public boolean isNameChangeable() {
            return true;
        }

        public boolean isValueChangeable() {
            return false;
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
        }

        public String getFilename() {
            return getValue().toString().split("/")[0]; //$NON-NLS-1$
        }
        
        public String getPath() {
            return getValue().toString().split("/")[1]; //$NON-NLS-1$
        }
        
    }

    public static class ArgumentsListNode extends Node {

        public ArgumentsListNode(Node parent, String nodeName, LslParam[] params) {
            super(parent, nodeName, null);
            for (int i = 0; i < params.length; i++) {
                addChild(new ArgumentNode(this,params[i].getName(), params[i].getType()));
            }
        }

        public String getNameDisplay() { return "Arguments"; }
        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return null;
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return false;
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
        }
    }
    
    public static class BindingListNode extends Node {
        private static final NodeFactory[] LEGAL_CHILDREN = new NodeFactory[] { BINDING_NODE_FACTORY };
        private String displayName;
        public BindingListNode(Node parent, String nodeName, String displayName) {
            super(parent, nodeName, null);
            this.displayName = displayName;
        }

        
        public String getNameDisplay() {
            return displayName;
        }


        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return null;
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return false;
        }

        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILDREN;
        }

        protected void onUpdate(String s) {
        }
        
    }
    
    private static final NodeFactory2 BINDING_NODE_FACTORY = new NodeFactory2() {

        public Node createNode(Node parent, String value) {
            SuiteNode root = (SuiteNode) parent.findRoot();
            TestNode test = (TestNode) parent.getParent();
            NameTypePair[] pairs = root.nature().getGlobalVariables(test.getFilename());
            for (int i = 0; i < pairs.length; i++) {
                if (value.equals(pairs[i].getName())) {
                    return new BindingNode(parent,pairs[i].getName(), pairs[i].getType());
                }
            }
            return null;
        }

        public String getNodeCreationId() {
            return "globvar";
        }

        public Node createNode(Node parent) {
            throw new UnsupportedOperationException();
        }

        public String getNodeTypeName() {
            return "Assignment";
        }
        
    };
    public static class BindingNode extends Node {
        private String type;
        public BindingNode(Node parent, String nodeName, String type) {
            super(parent, nodeName, LslTest.defaultValueFor(type));
            this.type = type;
        }

        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            String result = LslExpressionValidator.validateExpression(type, s);
            if (result == null) return NodeStatus.OK;
            return new NodeStatus(false, result);
        }

        public String getValueString() {
            return (String)getValue();
        }

        public boolean isDeletable() {
            return true;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return true;
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
            setValue(s);
        }
        
    }
    
    public static class ReturnNode extends Node {
        String type;
        public ReturnNode(Node parent, String name, String type, Object value) {
            super(parent, name , value);
            this.type = type;
        }
        
        public String getNameDisplay() {
            return "Returns (" + type + ")";
        }
        
        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            String result = LslExpressionValidator.validateExpression(type, s);
            if (result == null) return NodeStatus.OK;
            return new NodeStatus(false, result);
        }

        public String getValueString() {
            String s = (String)getValue();
            return s == null ? "" : s;
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return !"void".equals(type); //$NON-NLS-1$
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
            setValue(s);
        }
    }
    
    public static class ExpectedReturnNode extends ReturnNode {
        public ExpectedReturnNode(Node parent, String type, Object value) {
            super(parent, "returns" , type, value);
        }

        public NodeStatus checkValueString(String s) {
            if (s.trim().equals("")) return NodeStatus.OK;
            return super.checkValueString(s);
        }

        protected void onUpdate(String s) {
            super.onUpdate("".equals(s.trim())? null : s);
        }
        
    }

    public static class ArgumentNode extends Node {
        private String type;
        public ArgumentNode(Node parent, String nodeName, String type) {
            super(parent, nodeName, LslTest.defaultValueFor(type));
            this.type = type;
        }

        public String getNameDisplay() {
            return getName() + " (" + type + ") ";  //$NON-NLS-1$//$NON-NLS-2$
        }
        
        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            String result = LslExpressionValidator.validateExpression(type, s);
            if (result == null) return NodeStatus.OK;
            return new NodeStatus(false, result);
        }

        public String getValueString() {
            return (String)getValue();
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return true;
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
            setValue(s);
        }

        public void setType(String type) {
            this.type = type;
        }

        public String getType() {
            return type;
        }
        
    }

    public static class ExpectationsNode extends Node {
        private static final NodeFactory[] LEGAL_CHILDREN = { EXPECTED_CALL_FACTORY };
        public ExpectationsNode(Node parent) {
            super(parent, "expectations", null);
            addChild(new ExpectationsModeNode(this,"nice"));
        }

        public String getNameDisplay() {
            return "Call Expectations";
        }
        
        public String getMode() {
            ExpectationsModeNode node = (ExpectationsModeNode) findChildByName("mode");
            return node.getValueString();
        }
        
        public List getExpectedCalls() {
            return findChildrenByType(ExpectedCallNode.class);
        }
        
        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return null;
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return false;
        }

        public NodeFactory[] legalChildNodes() {
            return LEGAL_CHILDREN;
        }

        protected void onUpdate(String s) {
        }
        
    }

    public static class ExpectationsModeNode extends Node {

        public ExpectationsModeNode(Node parent, Object value) {
            super(parent, "mode", value);
        }

        public String getNameDisplay() { return "Call handler mode"; }
        
        public String getChoicesId() {
            return "expectations-mode";
        }

        public boolean hasValueChoices() {
            return true;
        }

        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return (String)getValue();
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return true;
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
            setValue(s);
        }
        
    }

    private static final NodeFactory2 EXPECTED_CALL_FACTORY = new NodeFactory2() {

        public Node createNode(Node parent, String value) {
            return new ExpectedCallNode(parent, "call", value);
        }

        public String getNodeCreationId() {
            return "call";
        }

        public Node createNode(Node parent) {
            throw new UnsupportedOperationException();
        }

        public String getNodeTypeName() {
            return "Call";
        }
        
    };
    public static class ExpectedCallNode extends Node {

        public ExpectedCallNode(Node parent, String nodeName, Object value) {
            super(parent, nodeName, value);
            final String name = (String)value;
            LslFunction func = (LslFunction) Util.find(new Predicate() {
                public boolean test(Object o) {
                    return name.equals(((LslFunction)o).getName());
                }
            }, LslPlusPlugin.getLLFunctions());
            
            addChild(new ExpectedArgumentsListNode(this,"args", func.getParams()));
            addChild(new ReturnNode(this,"returns", func.getReturns(),LslTest.defaultValueFor(func.getReturns())));
        }

        public ExpectedArgumentsListNode getArgumentListNode() {
            return (ExpectedArgumentsListNode) findChildByName("args");
        }
        
        public ReturnNode getReturn() {
            return (ReturnNode) findChildByName("returns");
        }
        
        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return (String) getValue();
        }

        public boolean isDeletable() {
            return true;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return false;
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
        }
        
    }

    public static class ExpectedArgumentsListNode extends Node {

        public ExpectedArgumentsListNode(Node parent, String nodeName, LslParam[] params) {
            super(parent, nodeName, null);
            for (int i = 0; i < params.length; i++) {
                addChild(new ExpectedArgumentNode(this,params[i].getName(), params[i].getType()));
            }
        }

        public NodeStatus checkNameString(String name) {
            return NodeStatus.OK;
        }

        public NodeStatus checkValueString(String s) {
            return NodeStatus.OK;
        }

        public String getValueString() {
            return null;
        }

        public boolean isDeletable() {
            return false;
        }

        public boolean isNameChangeable() {
            return false;
        }

        public boolean isValueChangeable() {
            return false;
        }

        public NodeFactory[] legalChildNodes() {
            return EMPTY_FACTORY_LIST;
        }

        protected void onUpdate(String s) {
        }
    }
    
    public static class ExpectedArgumentNode extends ArgumentNode {
     
        public ExpectedArgumentNode(Node parent, String nodeName, String type) {
            super(parent, nodeName, type);
        }

        public NodeStatus checkValueString(String s) {
            if (s.trim().equals("")) return NodeStatus.OK;
            return super.checkValueString(s);
        }

        public String getValueString() {
            if (getValue() == null) return "";
            return super.getValueString();
        }

        protected void onUpdate(String s) {
            
            super.onUpdate("".equals(s.trim()) ? null : s);
        }
    }
    
    private static XStream xstream = new XStream(new DomDriver());
    private static void configureXStream(XStream xstream) {
        Class[] nodeTypes = new Class[] {
                SuiteNode.class, TestNode.class, ArgumentNode.class,
                ArgumentsListNode.class, BindingListNode.class,
                BindingNode.class, ReturnNode.class, ExpectedReturnNode.class,
                ExpectationsNode.class, ExpectationsModeNode.class,
                ExpectedCallNode.class, ExpectedArgumentsListNode.class,
                ExpectedArgumentNode.class
        };
        
        xstream.omitField(Node.class, "parent"); //$NON-NLS-1$
        xstream.omitField(Node.class, "children"); //$NON-NLS-1$
        xstream.omitField(Node.class, "listeners"); //$NON-NLS-1$
        for (int i = 0; i < nodeTypes.length; i++) {
            Class c = nodeTypes[i];
            String name = c.getSimpleName();
            name = name.substring(0, 1).toLowerCase() + name.substring(1);
            xstream.alias(name, c);
        }
        
        xstream.omitField(SuiteNode.class, "resource"); //$NON-NLS-1$
    }
    
    static {
        configureXStream(xstream);
    }
    
    public static String toXml(SuiteNode world) {
        world.syncChildren();
        return xstream.toXML(world);
    }

    public static SuiteNode fromXml(InputStream contents, IFile file) {
        SuiteNode n = (SuiteNode) xstream.fromXML(contents);
        n.setResource(file);
        n.propagateParent();
        return n;
    }
    
    public static SuiteNode fromLslTestSuite(LslTestSuite suite) {
        SuiteNode suiteNode = new SuiteNode(null, suite.getResource().getProjectRelativePath().lastSegment());
        suiteNode.setResource((IFile)suite.getResource());
        for (int i = 0; i < suite.getTests().length; i++) {
            LslTest t = suite.getTests()[i];
            TestNode node = new TestNode(suiteNode,t.getName(), t.getEntryPoint().getFileName() +
                    "/" + t.getEntryPoint().getPath()); //$NON-NLS-1$
            suiteNode.addChild(node);
            
            ArgumentsListNode argsList = (ArgumentsListNode) node.findChildByName("args");
            List args = argsList.findChildrenByType(ArgumentNode.class);
            if (args.size() == t.getArguments().length) {
                int j = 0;
                for (Iterator it = args.iterator(); it.hasNext(); ) {
                    LslValue v = t.getArguments()[j++];
                    ArgumentNode argNode = (ArgumentNode) it.next();
                    argNode.setType(LslTest.lslTypeToString(v.getClass()));
                    argNode.setValue(v.toString());
                }
            }
            
            ExpectedReturnNode returns = (ExpectedReturnNode) node.findChildByName("returns");
            if (t.getExpectedReturn().getVal() != null) {
                returns.type = LslTest.lslTypeToString(t.getExpectedReturn().getVal().getClass());
            }
            returns.setValue(t.getExpectedReturn().getVal()== null ? "" : t.getExpectedReturn().getVal().toString());
            
            BindingListNode initial = (BindingListNode) node.findChildByName("initial");
            
            for (Iterator it = t.getInitialBindings().iterator(); it.hasNext(); ) {
                LslTest.GlobBinding binding = (GlobBinding) it.next();
                BindingNode bn = new BindingNode(initial, binding.getName(), LslTest.lslTypeToString(binding.getValue().getClass()));
                bn.setValue(binding.getValue().toString());
                initial.addChild(bn);
            }
            
            BindingListNode finals = (BindingListNode) node.findChildByName("final");
            
            for (Iterator it = t.getFinalBindings().iterator(); it.hasNext(); ) {
                LslTest.GlobBinding binding = (GlobBinding) it.next();
                BindingNode bn = new BindingNode(finals, binding.getName(), LslTest.lslTypeToString(binding.getValue().getClass()));
                bn.setValue(binding.getValue().toString());
                finals.addChild(bn);
            }
            
            ExpectationsNode expectations = (ExpectationsNode) node.findChildByName("expectations");
            ExpectationsModeNode expectationsMode = (ExpectationsModeNode) expectations.findChildByName("mode");
            expectationsMode.setValue(t.getExpectations().getMode());
            
            List expectedCalls = t.getExpectations().getExpectedCalls();
            
            for (Iterator it = expectedCalls.iterator(); it.hasNext(); ) {
                LslTest.ExpectedCall call = (ExpectedCall) it.next();
                ExpectedCallNode callNode = new ExpectedCallNode(expectations, "call", call.getName());
                ExpectedArgumentsListNode expectedArgs = (ExpectedArgumentsListNode) callNode.findChildByName("args");
                ReturnNode returnVal = (ReturnNode) callNode.findChildByName("returns");
                
                if (returnVal.type.equals(LslTest.lslTypeToString(call.getReturns().getClass()))) {
                    returnVal.setValue(call.getReturns().toString());
                }
                List callArgs = call.getArgs();
                List argNodes = expectedArgs.getChildren();
                Iterator it2 = argNodes.iterator();
                Iterator it1 = callArgs.iterator();
                while (it1.hasNext() && it2.hasNext()) {
                    LslTest.MaybeValue val = (MaybeValue) it1.next();
                    ExpectedArgumentNode expectNode = (ExpectedArgumentNode) it2.next();
                    String type = expectNode.getType();
                    if (val.getVal() == null) continue;
                    if (!LslTest.lslTypeToString(val.getVal().getClass()).equals(type)) break;
                    expectNode.setValue(val.getVal() == null ? "" : val.getVal().toString());
                }
                expectations.addChild(callNode);
            }
            
        }
        return suiteNode;
    }
    
    public static LslTestSuite toLslTestSuite(SuiteNode suiteNode) {
        LslTestSuite suite = new LslTestSuite();
        suite.setIResource(suiteNode.resource);
        
        List testNodes = suiteNode.getChildren();
        
        for (Iterator it = testNodes.iterator(); it.hasNext();) {
            TestNode tn = (TestNode) it.next();
            LslTest test = new LslTest();
            test.setSuite(suite);
            test.setName(tn.getName());
            EntryPoint ep = new EntryPoint();
            ep.setFileName(tn.getFilename());
            ep.setPath(tn.getPath());
            test.setEntryPoint(ep);
            ArgumentsListNode argumentsNode = tn.getArguments();
            List argumentsList = argumentsNode.getChildren();
            LslValue[] args = new LslValue[argumentsList.size()];
            for (int index = 0; index < args.length; index++) {
                ArgumentNode argNode = (ArgumentNode) argumentsList.get(index);
                args[index] = LslTest.mkLslType(argNode.getType(), argNode.getValueString());
            }
            
            test.setArguments(args);
            ExpectedReturnNode returnNode = tn.getReturn();
            
            if (returnNode.getValue() == null) {
                test.setExpectedReturn(new MaybeValue());
            } else {
                test.setExpectedReturn(new MaybeValue(LslTest.mkLslType(returnNode.type, returnNode.getValueString())));
            }
            
            test.setInitialBindings(convertBindings(tn.getInitialBindings()));
            test.setFinalBindings(convertBindings(tn.getFinalBindings()));
            
            ExpectationsNode expectationsNode = tn.getExpectations();
            CallExpectations expectations = new CallExpectations();
            expectations.setMode(expectationsNode.getMode());
            List expectedCallNodes = expectationsNode.getExpectedCalls();
            List expectedCalls = expectations.getExpectedCalls();
            for (Iterator expectIt = expectedCallNodes.iterator(); expectIt.hasNext();) {
                ExpectedCall call = new ExpectedCall();
                ExpectedCallNode callNode = (ExpectedCallNode) expectIt.next();
                
                List callArgs = call.getArgs();
                ExpectedArgumentsListNode expectedArgsNode = callNode.getArgumentListNode();
                List expectedArgNodes = expectedArgsNode.getChildren();
                for (Iterator argNodeIt = expectedArgNodes.iterator(); argNodeIt.hasNext();) {
                    ExpectedArgumentNode argNode = (ExpectedArgumentNode) argNodeIt.next();
                    if (argNode.getValue() == null) {
                        callArgs.add(new MaybeValue());
                    } else {
                        callArgs.add(new MaybeValue(LslTest.mkLslType(argNode.getType(), 
                                argNode.getValueString())));
                    }
                }
                call.setName(callNode.getValueString());
                
                ReturnNode returnVal = callNode.getReturn();
                call.setReturns(LslTest.mkLslType(returnVal.type, returnVal.getValueString()));
                expectedCalls.add(call);
            }
            test.setExpectations(expectations);
            suite.addTest(test);
        }
        return suite;
        
    }
    
    private static ArrayList convertBindings(BindingListNode node) {
        ArrayList bindings = new ArrayList();
        
        for (Iterator ib = node.getChildren().iterator(); ib.hasNext();) {
            BindingNode bindingNode = (BindingNode) ib.next();
            GlobBinding binding = new GlobBinding();
            binding.setName(bindingNode.getName());
            binding.setValue(LslTest.mkLslType(bindingNode.type, bindingNode.getValueString()));
            bindings.add(binding);
        }
        
        return bindings;
    }
}
