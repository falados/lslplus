package lslplus.editor;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.SortedSet;
import lslplus.LslPlusPlugin;
import lslplus.LslProjectNature;
import lslplus.language_metadata.LslFunction;
import lslplus.language_metadata.LslParam;
import lslplus.lsltest.LslTest;
import lslplus.lsltest.LslTestSuite;
import lslplus.lsltest.LslTest.CallExpectations;
import lslplus.lsltest.LslTest.EntryPoint;
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

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;

public class LslTestContentProvider implements ITreeContentProvider {
    public interface NodeChangeListener {
        public void changed(Object source);
    }
    
    public interface LslValueAddable {
        public void add(LslValue v);
    }
    
    public interface Emptyable {
        public void emptyContents();
        public boolean alreadyEmpty();
    }
    
    public abstract class Node {
        private HashSet listeners = new HashSet();
        private String name;
        private Node parent;
        private Object value;
        private ParentSetter setter;
        public Node(String name, Node parent, Object value, ParentSetter setter) {
            this.setName(name);
            this.setParent(parent);
            this.setValue(value);
            this.setter = setter;
        }
        
        public String displayString() {
            if (getValue() == null) return BLANK;
            return getValue().toString();
        }
        public abstract Node[] getChildren();
        public Object getEditableValue() {
            return displayString();
        }

        public String getName() {
            return name;
        }
        
        public abstract boolean hasChildren();
        
        public abstract boolean isEditable();
        
        public final boolean set(Object o) { 
            boolean result = setMyValue(o);
            if (result && setter != null) setter.set(getValue());
            return result;
         }
        
        protected abstract boolean setMyValue(Object o);
        
        public void setName(String name) {
            this.name = name;
        }
        
        public boolean isClearable() {
            return false;
        }
        
        public boolean isDeletable() {
            return false;
        }
        
        public void deleteChild(Node n) {
        }
        
        public void clear() {
        }
        
        public String isValid(Object o) {
            return null;
        }

        public void setValue(Object value) {
            this.value = value;
            fireNodeChange();
        }

        public Object getValue() {
            return value;
        }

        public void setParent(Node parent) {
            this.parent = parent;
        }

        public Node getParent() {
            return parent;
        }
        
        public void addListener(NodeChangeListener listener) {
            listeners.add(listener);
        }
        
        public void removeListener(NodeChangeListener listener) {
            listeners.remove(listener);
        }
        
        protected void fireNodeChange() {
            for (Iterator i = listeners.iterator(); i.hasNext();) {
                NodeChangeListener l = (NodeChangeListener) i.next();
                l.changed(this);
            }
        }
    }

	public class ArgumentsNode extends Node {
		private LslParam[] params;
		public ArgumentsNode(String name, Node parent, Object value, LslParam[] params,
		        ParentSetter setter) {
			super(name, parent, value, setter);
			this.params = params;
		}

		private LslValue[] getArgs() {
			LslValue[] currentArgs = (LslValue[]) getValue();
			if (currentArgs == null) currentArgs = new LslValue[0];
			LslValue[] newArgs = currentArgs;
			if (currentArgs.length != params.length) {
				newArgs = new LslValue[params.length];
			} 
			
			for (int i = 0; i < params.length; i++) {
				Class argType = LslTest.stringToLslType(params[i].getType());
				if (i < currentArgs.length &&
				    currentArgs[i] != null &&
				    currentArgs[i].getClass().equals(argType)) {
					newArgs[i] = currentArgs[i];
				} else {
					newArgs[i] = LslTest.defaultValueFor(argType); 
				}
			}
			
			set(newArgs);
			return newArgs;
		}

		protected boolean setMyValue(Object o) {
		    setValue(o);
		    return true;
		}
		
		public Node[] getChildren() {
			if (params == null) return null;
			Node[] children = new Node[params.length];

			LslValue[] args = getArgs();
			for (int i = 0; i < children.length; i++) {
				final int index = i;
				children[i] = createLslValueNode(params[i].getName(), this, args[i], args[i].getClass(),
						false, new ParentSetter() {
							public void set(Object o) {
								((LslValue[])getValue())[index] = (LslValue) o;
							} 
				});
			}
			return children;
		}
		public boolean hasChildren() {
			return params != null && params.length > 0;
		}

		public boolean isEditable() {
			return false;
		}

        public String displayString() {
            if (params == null) return "()"; //$NON-NLS-1$
            StringBuilder buf = new StringBuilder("("); //$NON-NLS-1$
            LslValue[] args = getArgs();
            String sep = BLANK;
            for (int i = 0; i < args.length; i++) {
                buf.append(sep).append(args[i].toString());
                sep = ","; //$NON-NLS-1$
            }
            buf.append(")"); //$NON-NLS-1$
            return buf.toString();
        }
	}

	public class CallListNode extends Node {
	    private List children;
        public CallListNode(String name, Node parent, Object value, ParentSetter setter) {
            super(name, parent, value, setter);
            children = new LinkedList();
            
            List calls = getCalls();
            for (Iterator i = calls.iterator(); i.hasNext(); ) {
                children.add(createCallNode((ExpectedCall)i.next()));
            }
        }

        private CallNode createCallNode(ExpectedCall call) {
            return new CallNode(Messages.getString("LslTestContentProvider.CALL"), this, call, null); //$NON-NLS-1$
        }
        
        private List getCalls() {
            return (List) getValue();
        }
        
        public Node[] getChildren() {
            return (Node[]) children.toArray(new Node[children.size()]);
        }

        public boolean hasChildren() {
            return getCalls().size() > 0;
        }

        public boolean isEditable() {
            return false;
        }

        protected boolean setMyValue(Object o) {
            setValue(o);
            return true;
        }

        public String displayString() {
            return BLANK;
        }

        public void deleteChild(Node n) {
            ExpectedCall cn = (ExpectedCall) n.getValue();
            children.remove(n);
            getCalls().remove(cn);
        }
        
        public void addChild(ExpectedCall call) {
            getCalls().add(call);
            children.add(createCallNode(call));
        }
	}
	
	public class CallNode extends Node implements NodeChangeListener {
        public class CallNameNode extends Node implements ChoiceProvider {
            public CallNameNode(String name, Node parent, Object value,
                    ParentSetter setter) {
                super(name, parent, value, setter);
            }

            public Node[] getChildren() { return null; }

            public String[] getChoices() {
                return LslTestEditor.getStatefulFunctions();
            }

            public boolean hasChildren() { return false; }

            public boolean isEditable() {
                String[] choices = getChoices();
                return choices != null && choices.length > 0;
            }

            protected boolean setMyValue(Object o) {
                Integer i = (Integer) o;
                String name = LslTestEditor.getStatefulFunctions()[i.intValue()];
                if (!name.equals(getName())) return false;
                setValue(name);
                return true;
            }
        }

        public CallNode(String name, Node parent, Object value, ParentSetter setter) {
            super(name, parent, value, setter);
        }
	    private ExpectedCall getCall() {
	        return (ExpectedCall) getValue();
	    }
	    
        public Node[] getChildren() {
            Node[] children = new Node[3];
            
            children[0] = new CallNameNode(Messages.getString("LslTestContentProvider.NAME"), this,getCall().getName(),  //$NON-NLS-1$
                new ParentSetter() {
                    public void set(Object o) {
                        String name = (String) o;
                        if (!name.equals(getCall().getName())) {
                            getCall().setName(name);
                            LslFunction func = findFunction(name);
                            Class returnType = LslTest.stringToLslType(func.getReturns());
                            getCall().setReturns(LslTest.defaultValueFor(returnType));
                            int numParams = func.getParams().length;
                            getCall().getArgs().clear();
                            for (int i = 0; i < numParams; i++) {
                                getCall().getArgs().add(new MaybeValue());
                            }
                            fTreeViewer.refresh(CallNode.this);
                        }
                    }
            });
            
            LslFunction func = findFunction(getCall().getName());

            Class returnType;
            LslParam[] params;
            if (func == null) {
                returnType = LslValue.class;
                params = new LslParam[0];
            } else {
                returnType = BLANK.equals(func.getReturns()) ? LslVoid.class : LslTest.stringToLslType(func.getReturns());
                params = func.getParams();
            }
            
            children[1] = new CallArgs(Messages.getString("LslTestContentProvider.ARGUMENTS"),this,getCall().getArgs(), //$NON-NLS-1$
                    new ParentSetter() {
                        public void set(Object o) {
                            getCall().setArgs((List)o);
                        }
            }, params);
            children[1].addListener(this);
            children[2] = createLslValueNode(Messages.getString("LslTestContentProvider.RETURNS"), this, getCall().getReturns(), returnType, false, //$NON-NLS-1$
                    new ParentSetter() {
                        public void set(Object o) {
                            getCall().setReturns((LslValue)o);
                        }
            });
            
            return children;
        }

        public boolean hasChildren() {
            return true;
        }

        public boolean isEditable() {
            return false;
        }

        protected boolean setMyValue(Object o) {
            setValue(o);
            return true;
        }

        public String displayString() {
            String name = getCall().getName();
            if (name == null) return BLANK;
            StringBuilder builder = new StringBuilder(name).append('(');
            String sep = ""; //$NON-NLS-1$
            if (getCall().getArgs() != null) {
                for (Iterator i = getCall().getArgs().iterator(); i.hasNext(); ) {
                    MaybeValue v = (MaybeValue) i.next();
                    builder.append(sep);
                    if (v.getVal() == null) {
                        builder.append('_');
                    } else {
                        builder.append(v.getVal().toString());
                    }
                    
                    sep = ","; //$NON-NLS-1$
                }
            }

            builder.append(')');
            return builder.toString();
        }
        
        public void changed(Object source) {
            fTreeViewer.update(this, null);
        }
        
        public boolean isDeletable() {
            return true;
        }
	}
	
	public class CallArgs extends Node implements NodeChangeListener {
	    private LslParam[] params;
        public CallArgs(String name, Node parent, Object value, ParentSetter setter,
                LslParam[] params) {
            super(name, parent, value, setter);
            this.params = params;
        }

        private List getArgs() {
            return (List) getValue();
        }
        
        public Node[] getChildren() {
            List args = getArgs();
            Node[] children = new Node[args.size()];
            for (int i = 0; i < args.size(); i++) {
                final int index = i;
                children[i] = maybeNodeFactory(params[i].getName(), 
                        this, args.get(i), 
                        new ParentSetter() {
                            public void set(Object o) {
                                getArgs().set(index, o);
                            }
                }, LslTest.stringToLslType(params[i].getType()));
                children[i].addListener(this);
            }
            
            return children;
        }

        public boolean hasChildren() {
            return getArgs().size() > 0;
        }

        public boolean isEditable() {
            return false;
        }

        protected boolean setMyValue(Object o) {
            setValue(o);
            return false;
        }
        
        public String displayString() {
            return BLANK;
        }

        public void changed(Object source) {
            fireNodeChange();
        }
	}
	
	public interface ChoiceProvider {
		public String[] getChoices();
	}
	
	public class EntryPointNode extends Node {
		public EntryPointNode(String name, Node parent, Object value,
				ParentSetter setter) {
			super(name, parent, value, setter);
		}
		
		protected boolean setMyValue(Object o) {
		    if (Util.safeEquals(o, getValue())) return false;
		    setValue(o);
		    return true;
		}
		
		public Object getEditableValue() {
		    return getValue();
		}
		
		public String displayString() {
			if (getValue() == null) return super.displayString();
			
			EntryPoint entry = (EntryPoint) getValue();
			if (entry.getFileName() == null) return Messages.getString("LslTestContentProvider.NOT_SET"); //$NON-NLS-1$
			if (entry.getPath() == null) return entry.getFileName() + Messages.getString("LslTestContentProvider.SLASH_QUESTION"); //$NON-NLS-1$
			return entry.getFileName() + "/" + entry.getPath(); //$NON-NLS-1$
		}
		
		public LslParam[] getArgumentInfo() {
			EntryPoint ep = (EntryPoint)getValue();
			
			if (ep != null && ep.getFileName() != null && ep.getPath() != null) {
				return nature.getParams(ep.getFileName(), ep.getPath());
			}
			
			return null;
		}
		
		public Node[] getChildren() {
			return null;
		}
		
		public Class getReturnType() {
			EntryPoint ep = (EntryPoint)getValue();
			
			if (ep != null && ep.getFileName() != null && ep.getPath() != null) {
				String s = nature.getReturnType(ep.getFileName(), ep.getPath());
				
				return LslTest.stringToLslType(s);
			}
			
			return LslValue.class;
		}
		
		public boolean hasChildren() { return false; }
		
		public boolean isEditable() { return true; }
	}

	public class ExpectationsModeNode extends Node implements ChoiceProvider {
        public ExpectationsModeNode(String name, Node parent, Object value, ParentSetter setter) {
            super(name, parent, value, setter);
        }

        public Node[] getChildren() {
            return null;
        }

        public boolean hasChildren() {
            return false;
        }

        public boolean isEditable() {
            return true;
        }

        protected boolean setMyValue(Object o) {
            Integer i = (Integer) o;
            String s = getChoices()[i.intValue()];
            setValue(s);
            return true;
        }

        public String[] getChoices() {
            SortedSet modes = LslTest.CallExpectations.getModes();
            
            return (String[]) modes.toArray(new String[modes.size()]);
        }
	}
	public class ExpectationsNode extends Node {

        public ExpectationsNode(String name, Node parent, Object value, ParentSetter setter) {
            super(name, parent, value, setter);
        }

        private CallExpectations getCallExpectations() {
            return (CallExpectations) getValue();
        }
        public Node[] getChildren() {
            Node[] children = new Node[2];
            
            children[0] = new ExpectationsModeNode(Messages.getString("LslTestContentProvider.CALLS_MODE"), this,  //$NON-NLS-1$
                    getCallExpectations().getMode(), new ParentSetter() {
                        public void set(Object o) {
                            getCallExpectations().setMode((String)o);
                        }
            });
            
            children[1] = new CallListNode(Messages.getString("LslTestContentProvider.CALLS"),this,  //$NON-NLS-1$
                    getCallExpectations().getExpectedCalls(), new ParentSetter() {
                        public void set(Object o) {
                            throw new RuntimeException(Messages.getString("LslTestContentProvider.SHOULDNT_GET_CALLED")); //$NON-NLS-1$
                        }
            });
            return children;
        }

        public boolean hasChildren() {
            return true;
        }

        public boolean isEditable() {
            return false;
        }

        protected boolean setMyValue(Object o) {
            setValue(o);
            return true;
        }
	    
        public String displayString() {
            return BLANK;
        }
	}
	public class FloatComponentNode extends Node {

		public FloatComponentNode(String name, Node parent, Object value, ParentSetter setter) {
			super(name, parent, value, setter);
		}

		public String displayString() {
			if (getValue() == null) return BLANK;
			return getValue().toString();
		}

		public Node[] getChildren() {
			return null;
		}

		public boolean hasChildren() {
			return false;
		}

		public boolean isEditable() {
			return true;
		}
		
		protected boolean setMyValue(Object o) {
			setValue(Float.valueOf(o.toString()));
			return true;
		}
		
		public String isValid(Object o) {
			try {
				Float.parseFloat(o.toString());
				return null;
			} catch (NumberFormatException e) {
				return Messages.getString("LslTestContentProvider.NOT_VALID_FLOAT"); //$NON-NLS-1$
			}
		}
	}

	public class GlobalBindingListNode extends GlobalBindingNode implements LslValueAddable, Emptyable {

        public GlobalBindingListNode(String name, Node parent, Object value, ParentSetter setter) {
            super(name, parent, value, setter, LslList.class);
        }

        public void add(LslValue v) {
            LslList list = getListVal();
            if (list == null) {
                set(list = new LslList(new LinkedList()));
            }
            
            list.getVal().add(v);
        }

        private LslList getListVal() {
            return (LslList) getGlobalBinding().getValue();
        }

        public void emptyContents() {
            getListVal().getVal().clear();
            fireNodeChange();
        }

        public boolean alreadyEmpty() {
            return getListVal().getVal().isEmpty();
        }

    }

	public class GlobalBindingNode extends Node implements NodeChangeListener {
	    private LslValueNode delegateNode;
        public GlobalBindingNode(String name, Node parent, Object value, ParentSetter setter, Class type) {
            super(name, parent, value, setter);
            ParentSetter mySetter = new ParentSetter() {
                public void set(Object o) {
                    getGlobalBinding().setValue((LslValue)o);
                }
            };
            
            LslValue val = getGlobalBinding().getValue();
            if (type.equals(LslList.class)) {
                delegateNode = new LslListNode(name, parent, val, false, mySetter);
            } else if (type.equals(LslVector.class)) {
                delegateNode = new LslVectorNode(name, parent, val, false, mySetter);
            } else if (type.equals(LslRotation.class)) {
                delegateNode = new LslRotationNode(name, parent, val, false, mySetter);
            } else if (type.equals(LslString.class)) {
                delegateNode = new LslStringNode(name, parent, val, false, mySetter);
            } else if (type.equals(LslInteger.class)) {
                delegateNode = new LslIntegerNode(name, parent, val, false, mySetter);
            } else if (type.equals(LslFloat.class)) {
                delegateNode = new LslFloatNode(name, parent, val, false, mySetter);
            } else if (type.equals(LslKey.class)) {
                delegateNode = new LslKeyNode(name, parent, val, false, mySetter);
            } else if (type.equals(LslVoid.class)) {
                delegateNode = new LslVoidNode(name, parent, val, mySetter);
            } else {
                delegateNode = new LslUnknownValueNode(name, parent, mySetter);
            }
            
            delegateNode.addListener(this);
        }

        public Node[] getChildren() {
            return delegateNode.getChildren();
        }

        public boolean hasChildren() {
            return delegateNode.hasChildren();
        }

        public boolean isEditable() {
            return delegateNode.isEditable();
        }

        public boolean isClearable() { return false; }
        protected boolean setMyValue(Object o) {
            if (delegateNode.set(o)) {
                fireNodeChange();
                return true;
            }
            return false;
        }

        public String displayString() {
            return delegateNode.displayString();
        }

        public Object getEditableValue() {
            return delegateNode.getEditableValue();
        }

        public String isValid(Object o) {
            return delegateNode.isValid(o);
        }

        public boolean isDeletable() {
            return true;
        }

        public void changed(Object source) {
            fTreeViewer.refresh(this);
        }
        
        protected GlobBinding getGlobalBinding() { return (GlobBinding) getValue(); }
	}

	public class GlobalBindingsNode extends Node implements Emptyable {

        private boolean initial;

        private LinkedList children;
        public GlobalBindingsNode(String name, Node parent, Object value, ParentSetter setter,
                boolean initial) {
            super(name, parent, value, setter);
            this.initial = initial;
            
            children = new LinkedList();
            List bindings = getGlobalBindings();
            for (Iterator i = bindings.iterator(); i.hasNext(); ) {
                GlobBinding binding = (GlobBinding) i.next();
                addBindingNode(children, binding);
            }
        }

        private void addBindingNode(LinkedList children, GlobBinding binding) {
            if (binding.getValue() instanceof LslList) {
                children.add(new GlobalBindingListNode(binding.getName(), this, binding, 
                        new ParentSetter() {
                            public void set(Object o) {
                            }
                }));
            } else {
                children.add(new GlobalBindingNode(binding.getName(), this, binding,
                        new ParentSetter() {
                            public void set(Object o) {
                            }
                    
                }, binding.getValue().getClass()));
            }
        }

        private List getGlobalBindings() { return (List) getValue(); }
        
        public Node[] getChildren() {
            return (Node[]) children.toArray(new Node[children.size()]);
        }

        public boolean hasChildren() {
            return !children.isEmpty();
        }

        public boolean isEditable() {
            return false;
        }

        protected boolean setMyValue(Object o) {
            setValue(o);
            return true;
        }

        public boolean alreadyEmpty() {
            return getGlobalBindings().isEmpty();
        }

        public void emptyContents() {
            children.clear();
            getGlobalBindings().clear();
        }
        
        public String displayString() { return BLANK; }

        public void deleteChild(Node n) {
            children.remove(n);
            GlobBinding gb = (GlobBinding) n.getValue();
            List gbs = getGlobalBindings();
            gbs.remove(gb);
        }
        
        public void addChild(GlobBinding binding) {
            getGlobalBindings().add(binding);
            addBindingNode(children, binding);
        }

        public boolean isInitial() {
            return initial;
        }
	}
	
	public class LslFloatNode extends LslValueNode {

		public LslFloatNode(String name, Node parent, Object value, boolean clearable,
				ParentSetter setter) {
			super(name, parent, value, LslFloat.class, clearable, setter);
		}

		public Node[] getChildren() {
			return null;
		}

		public String getTypeName() { return "float"; } //$NON-NLS-1$

		public boolean hasChildren() {
			return false;
		}
		
		public boolean isEditable() {
			return true;
		}
		
        public String isValid(Object o) {
            return LslPlusPlugin.validateExpression("<expression><type>float</type><text>" + //$NON-NLS-1$
                    o.toString() + "</text></expression>"); //$NON-NLS-1$
        }

		
		protected boolean setMyValue(Object o) {
			setValue(new LslFloat(o.toString()));
			return true;
		}
	}
	
	public class LslIntegerNode extends LslValueNode {

		public LslIntegerNode(String name, Node parent, Object value, boolean clearable,
				ParentSetter setter) {
			super(name, parent, value, LslInteger.class, clearable, setter);
		}

		public Node[] getChildren() {
			return null;
		}

		public String getTypeName() { return "integer"; } //$NON-NLS-1$

		public boolean hasChildren() {
			return false;
		}
		
		public boolean isEditable() {
			return true;
		}
		public String isValid(Object o) {
		    return LslPlusPlugin.validateExpression("<expression><type>integer</type><text>" + //$NON-NLS-1$
		            o.toString() + "</text></expression>"); //$NON-NLS-1$
		}
		
		protected boolean setMyValue(Object o) {
			setValue(new LslInteger(o.toString()));
			return true;
		}
	}
	
	public class LslKeyNode extends LslValueNode {
		public LslKeyNode(String name, Node parent, Object value, boolean clearable,
		        ParentSetter setter) {
			super(name, parent, value, LslString.class, clearable, setter);
		}

		public Node[] getChildren() {
			return null;
		}

		public String getTypeName() {
			return "key"; //$NON-NLS-1$
		}

		public boolean hasChildren() {
			return false;
		}

		public boolean isEditable() {
			return true;
		}
		
        public String isValid(Object o) {
            return LslPlusPlugin.validateExpression("<expression><type>key</type><text>" + //$NON-NLS-1$
                    o.toString() + "</text></expression>"); //$NON-NLS-1$
        }

        protected boolean setMyValue(Object o) {
			setValue(new LslKey(o.toString()));
			return true;
		}
	}
	
	public class LslListNode extends LslValueNode implements LslValueAddable, Emptyable {

		public LslListNode(String name, Node parent, Object value, boolean clearable,
		        ParentSetter setter) {
			super(name, parent, value, LslList.class, clearable, setter);
		}

		public Node[] getChildren() {
			if (getValue() == null) return null;
			LslList list = (LslList) getValue();
			LinkedList list1 = new LinkedList();
			int k = 0;
			for (Iterator i = list.getVal().iterator(); i.hasNext(); ) {
				LslValue val = (LslValue) i.next();
				final int index = k++;
				Node node = createLslValueNode(Messages.getString("LslTestContentProvider.ELEMENT") + k, this, val, val.getClass(), //$NON-NLS-1$
						isClearable(), new ParentSetter() {
							public void set(Object o) {
								((LslList)getValue()).getVal().set(index, o);
								LslListNode.this.fireNodeChange();
							}
				});
				list1.add(node);
			}
			
			return (Node[]) list1.toArray(new LslValueNode[list1.size()]);
		}

		public String getTypeName() {
			return "list"; //$NON-NLS-1$
		}

		public boolean hasChildren() {
			return getValue() != null && ((LslList)getValue()).getVal().size() > 0;
		}

		public boolean isEditable() {
			return false;
		}

		protected boolean setMyValue(Object o) {
		    setValue(o);
		    return true;
		}

        public void add(LslValue v) {
            LslList l = (LslList) getValue();
            if (l == null) {
                set(l = new LslList(new LinkedList()));
            }
            
            l.getVal().add(v);
        }

        public void deleteChild(Node n) {
            LslValue val = (LslValue) n.getValue();
            if (val != null) {
                ((LslList)getValue()).getVal().remove(val);
                fireNodeChange();
            }
        }

        public void emptyContents() {
            ((LslList)getValue()).getVal().clear();
            fireNodeChange();
        }

        public boolean alreadyEmpty() {
            return ((LslList)getValue()).getVal().isEmpty();
        }

	}

	public class LslRotationNode extends LslValueNode {
		public LslRotationNode(String name, Node parent, Object value, boolean clearable,
		        ParentSetter setter) {
			super(name, parent, value, LslRotation.class, clearable, setter);
		}
		
		public Node[] getChildren() {
			Float xval = null, yval = null, zval = null, sval = null;
			LslRotation rotation = (LslRotation) getValue();
			if (rotation != null) {
				xval = new Float(rotation.x);
				yval = new Float(rotation.y);
				zval = new Float(rotation.z);
				sval = new Float(rotation.s);
			}
			
			FloatComponentNode[] children = new FloatComponentNode[3];
			
			children[0] = new FloatComponentNode("x", this, xval, new ParentSetter() { //$NON-NLS-1$
				public void set(Object o) {
					getOrInitValue().x = ((Float)o).floatValue();
				}
			});
			children[1] = new FloatComponentNode("y", this, yval, new ParentSetter() { //$NON-NLS-1$
				public void set(Object o) {
					getOrInitValue().y = ((Float)o).floatValue();
				}
			});
			children[2] = new FloatComponentNode("z", this, zval, new ParentSetter() { //$NON-NLS-1$
				public void set(Object o) {
					getOrInitValue().z = ((Float)o).floatValue();
				}
			});
			children[3] = new FloatComponentNode("s", this, sval, new ParentSetter() { //$NON-NLS-1$
				public void set(Object o) {
					getOrInitValue().s = ((Float)o).floatValue();
				}
			});
			return children;
		}
		
		private LslRotation getOrInitValue() {
			if (getValue() == null) {
				set(new LslRotation(0,0,0,0));
			}
			return (LslRotation) getValue();
		}
		
		public String getTypeName() {
			return "rotation"; //$NON-NLS-1$
		}

		public boolean hasChildren() { return true; }

		public boolean isEditable() {
			return false;
		}
		
		protected boolean setMyValue(Object o) {
		    this.setValue(o);
		    return true;
		}
	}
	
	public class LslStringNode extends LslValueNode {

		public LslStringNode(String name, Node parent, Object value, boolean clearable, 
		        ParentSetter setter) {
			super(name, parent, value, LslString.class, clearable, setter);
		}

		public Node[] getChildren() {
			return null;
		}

		public Object getEditableValue() {
			if (getValue() == null) return BLANK;
			else return ((LslString)getValue()).val;
		}

		public String getTypeName() {
			return "string"; //$NON-NLS-1$
		}
		
		public boolean hasChildren() {
			return false;
		}
		
		public boolean isEditable() {
			return true;
		}

        public String isValid(Object o) {
            return LslPlusPlugin.validateExpression("<expression><type>string</type><text>" + //$NON-NLS-1$
                    o.toString() + "</text></expression>"); //$NON-NLS-1$
        }

		protected boolean setMyValue(Object o) {
			setValue(new LslString(o.toString()));
			return true;
		}
	}

	public class LslUnknownValueNode extends LslValueNode {

		public LslUnknownValueNode(String name, Node parent, ParentSetter setter) {
			super(name, parent, null, LslValue.class, false, setter);
		}

		public Node[] getChildren() {
			return null;
		}

		public String getTypeName() { return Messages.getString("LslTestContentProvider.UNKNOWN"); } //$NON-NLS-1$

		public boolean hasChildren() {
			return false;
		}
		
		public boolean isEditable() {
			return false;
		}
		
		protected boolean setMyValue(Object o) {
		    return false;
		}
	}
	
	public abstract class LslValueNode extends Node {
		private boolean clearable;
		private Class type;
		public LslValueNode(String name, Node parent, Object value, Class type,	boolean clearable, 
		        ParentSetter setter) {
			super(name, parent, value, setter);
			this.clearable = clearable;
			this.type = type;
		}
		public boolean isClearable() { return clearable; }
		
		public void clear() {
		    if (isClearable()) {
		        set(null);
		    }
		}
		
		public String displayString() {
			if (getValue() == null) return Messages.getString("LslTestContentProvider.DONT_CARE"); //$NON-NLS-1$
			LslValue v = (LslValue) getValue();
			return v.toString();
		}
		
		abstract public Node[] getChildren();
		
		public String getName() {
			return super.getName() + " (" + getTypeName() + ")";  //$NON-NLS-1$//$NON-NLS-2$
		}
		public abstract String getTypeName();
		
		abstract public boolean hasChildren();
		
		abstract public boolean isEditable();
		
		public boolean isDeletable() {
		    return getParent() instanceof LslListNode;
		}
		
		public Object getEditableValue() {
		    if (getValue() == null) {
		        return LslTest.defaultValueFor(type).toString();
		    } else {
		        return getValue().toString();
		    }
        }
        protected boolean setMyValue(Object o) {
			this.setValue(o);
		    return true;
		}
	}
	
	public class LslVectorNode extends LslValueNode {
		public LslVectorNode(String name, Node parent, Object value, boolean clearable,
		        ParentSetter setter) {
			super(name, parent, value, LslVector.class, clearable, setter);
		}
		
		public Node[] getChildren() {
			Float xval = null, yval = null, zval = null;
			LslVector vector = (LslVector) getValue();
			if (vector != null) {
				xval = new Float(vector.x);
				yval = new Float(vector.y);
				zval = new Float(vector.z);
			}
			
			FloatComponentNode[] children = new FloatComponentNode[3];
			
			children[0] = new FloatComponentNode("x", this, xval, new ParentSetter() { //$NON-NLS-1$
				public void set(Object o) {
					getOrInitVectorValue().x = ((Float)o).floatValue();
				}
			});
			children[1] = new FloatComponentNode("y", this, yval, new ParentSetter() { //$NON-NLS-1$
				public void set(Object o) {
					getOrInitVectorValue().y = ((Float)o).floatValue();
				}
			});
			children[2] = new FloatComponentNode("z", this, zval, new ParentSetter() { //$NON-NLS-1$
				public void set(Object o) {
					getOrInitVectorValue().z = ((Float)o).floatValue();
				}
			});
			return children;
		}
		
		private LslVector getOrInitVectorValue() {
			if (getValue() == null) {
				set(new LslVector(0,0,0));
			}
			return (LslVector) getValue();
		}
		
		public String getTypeName() {
			return "vector"; //$NON-NLS-1$
		}

		public boolean hasChildren() { return true; }

		public boolean isEditable() {
			return false;
		}
		
	}
	
	public class LslVoidNode extends LslValueNode {

		public LslVoidNode(String name, Node parent, Object value, ParentSetter setter) {
			super(name, parent, value, LslVoid.class, false, setter);
		}

		public String displayString() {
			return Messages.getString("LslTestContentProvider.NOT_APPLICABLE"); //$NON-NLS-1$
		}

		public Node[] getChildren() {
			return null;
		}

		public String getTypeName() { return Messages.getString("LslTestContentProvider.NONE"); } //$NON-NLS-1$

		public boolean hasChildren() {
			return false;
		}
	
		public boolean isEditable() {
			return false;
		}
	}
	
	public class MaybeListNode extends MaybeNode implements LslValueAddable, Emptyable, NodeChangeListener {

        public MaybeListNode(String name, Node parent, Object value, ParentSetter setter) {
            super(name, parent, value, setter, LslList.class);
        }

        public void add(LslValue v) {
            LslList list = getListVal();
            if (list == null) {
                set(list = new LslList(new LinkedList()));
            }
            
            list.getVal().add(v);
        }

        private LslList getListVal() {
            return (LslList) ((MaybeValue)getValue()).getVal();
        }

        public void emptyContents() {
            LslList val = (LslList) ((MaybeValue)getValue()).getVal();
            if (val != null) {
                val.getVal().clear();
                fireNodeChange();
            }
        }

        public boolean alreadyEmpty() {
            LslList val = (LslList) ((MaybeValue)getValue()).getVal();
            return val == null || val.getVal().isEmpty();
        }

	}
	
	public class MaybeNode extends Node implements NodeChangeListener {
	    private LslValueNode delegateNode;
	    private Class type;
        public MaybeNode(String name, Node parent, Object value, ParentSetter setter, Class type) {
            super(name, parent, value, setter);
            
            ParentSetter mySetter = new ParentSetter() {
                public void set(Object o) {
                    getMaybeValue().setVal((LslValue)o);
                }
            };
            
            this.type = type;
            LslValue val = null;
            if (value != null) {
                val = ((MaybeValue)value).getVal();
            }
            if (type.equals(LslList.class)) {
                delegateNode = new LslListNode(name, parent, val, false, mySetter);
            } else if (type.equals(LslVector.class)) {
                delegateNode = new LslVectorNode(name, parent, val, false, mySetter);
            } else if (type.equals(LslRotation.class)) {
                delegateNode = new LslRotationNode(name, parent, val, false, mySetter);
            } else if (type.equals(LslString.class)) {
                delegateNode = new LslStringNode(name, parent, val, false, mySetter);
            } else if (type.equals(LslInteger.class)) {
                delegateNode = new LslIntegerNode(name, parent, val, false, mySetter);
            } else if (type.equals(LslFloat.class)) {
                delegateNode = new LslFloatNode(name, parent, val, false, mySetter);
            } else if (type.equals(LslKey.class)) {
                delegateNode = new LslKeyNode(name, parent, val, false, mySetter);
            } else if (type.equals(LslVoid.class)) {
                delegateNode = new LslVoidNode(name, parent, val, mySetter);
            } else {
                delegateNode = new LslUnknownValueNode(name, parent, mySetter);
            }
            
            delegateNode.addListener(this);
        }

        private MaybeValue getMaybeValue() {
            return (MaybeValue) getValue();
        }
        
        public Class getMaybeType() {
            return type;
        }
        
        private boolean hasValue() {
            return getMaybeValue() != null &&
                   getMaybeValue().getVal() != null;
        }
        public Node[] getChildren() {
            if (!hasValue()) return null;
            return delegateNode.getChildren();
        }

        public boolean hasChildren() {
            if (!hasValue()) return false;
            return delegateNode.hasChildren();
        }

        public boolean isEditable() {
            return delegateNode.isEditable();
        }

        public boolean isClearable() { return true; }
        protected boolean setMyValue(Object o) {
            if (delegateNode.set(o)) {
                fireNodeChange();
                return true;
            }
            return false;
        }

        public void clear() {
            if (getMaybeValue() != null) {
                getMaybeValue().setVal(null);
                delegateNode.setValue(null);
                fireNodeChange();
            }
        }

        public String displayString() {
            if (getMaybeValue() == null ||
                getMaybeValue().getVal() == null) return Messages.getString("LslTestContentProvider.DONT_CARE"); //$NON-NLS-1$
            return delegateNode.displayString();
        }

        public Object getEditableValue() {
            return delegateNode.getEditableValue();
        }

        public String isValid(Object o) {
            if (o == null) return null;
            return delegateNode.isValid(o);
        }

        public void changed(Object source) {
            fTreeViewer.refresh(this);
        }
	    
	}
	
	public interface ParentSetter {
		public void set(Object o);
	}
	
	public class SuiteNode extends Node {
	    private LinkedList children;
		public SuiteNode(String name, Node parent, Object value) {
			super(name, parent, value, null);
			children = new LinkedList();
			LslTestSuite suite = (LslTestSuite) getValue();
			
			LslTest[] tests = suite.getTests();
			for (int i = 0; i < tests.length; i++) {
			    children.add(new TestNode(Messages.getString("LslTestContentProvider.TEST"), this, tests[i], null)); //$NON-NLS-1$
			}
		}
		
		public String displayString() {
			
			return LslTestContentProvider.this.suiteName;
		}
		
		public Node[] getChildren() {
//			LslTestSuite suite = (LslTestSuite) getValue();
//			Node[] children = new Node[suite.getTests().length];
//			
//			for (int i = 0; i < children.length; i++) {
//			    final int index = i;
//				children[i] = new TestNode("test", this, suite.getTests()[i], null);
//			}
			
			return (Node[]) children.toArray(new Node[children.size()]);
		}
		
		public boolean hasChildren() {
		    return !children.isEmpty();
		}
		
		public boolean isEditable() { return false; }
		
		protected boolean setMyValue(Object o) {
		    return false;
		}

        public void deleteChild(Node n) {
            if (n.getValue() instanceof LslTest) {
                LslTestSuite suite = (LslTestSuite) getValue();
                suite.removeTest((LslTest)n.getValue());
                children.remove(n);
            }
        }

        public void addTest(LslTest t) {
            ((LslTestSuite)getValue()).addTest(t);
            children.add(new TestNode(Messages.getString("LslTestContentProvider.TEST"), this, t, null)); //$NON-NLS-1$
        }
	}
	
	public class TestNode extends Node {
	    private LinkedList children = new LinkedList();
		public TestNode(String name, Node parent, Object value, ParentSetter setter) {
			super(name, parent, value, setter);
			buildChildList();
		}

		private void updateEntryPoint(EntryPoint ep) {
		    LslTest test = (LslTest)this.getValue();
		    if (Util.safeEquals(test.getEntryPoint(), ep)) return;
		    
		    test.setEntryPoint(ep);

		    buildChildList();
		    fTreeViewer.refresh(this);
		    fTreeViewer.expandToLevel(children.get(0), 0);
		}
		
		private EntryPointNode createEntryPointNode(EntryPoint ep) {
		    return new EntryPointNode(Messages.getString("LslTestContentProvider.ENTRY_POINT"), this, ep, new ParentSetter() { //$NON-NLS-1$
                public void set(Object o) {
                    updateEntryPoint((EntryPoint)o);
                }
		    });
		}

		private ArgumentsNode createArgumentsNode(LslValue[] args, LslParam[] params) {
		    return new ArgumentsNode(Messages.getString("LslTestContentProvider.ARGUMENTS"), this, args, params, //$NON-NLS-1$
		            new ParentSetter() {
                        public void set(Object o) {
                            ((LslTest)getValue()).arguments = (LslValue[]) o;
                        }
		    });
		}
		
		private MaybeNode createReturnNode(MaybeValue val, Class returnType) {
		    return maybeNodeFactory(Messages.getString("LslTestContentProvider.RETURNS"), this, val, new ParentSetter() { //$NON-NLS-1$
                public void set(Object o) {
                    ((LslTest)getValue()).setExpectedReturn((MaybeValue)o);
                }
		        
		    }, returnType);
		}
		private void buildChildList() {
            LslTest test = (LslTest)this.getValue();
            if (test == null) test = new LslTest();
            LinkedList l = new LinkedList();
            EntryPointNode entryPointNode = createEntryPointNode(test.getEntryPoint());
            l.add(createEntryPointNode(test.getEntryPoint()));
            Class returnType = entryPointNode.getReturnType();
            LslParam[] params = entryPointNode.getArgumentInfo();
            l.add(createArgumentsNode(test.arguments, params));
            l.add(createReturnNode(test.getExpectedReturn(), returnType));
            l.add(new GlobalBindingsNode(Messages.getString("LslTestContentProvider.INITIAL_GLOBALS"), this, test.getInitialBindings(), //$NON-NLS-1$
                    new ParentSetter() {
                        public void set(Object o) {
                            ((LslTest)(TestNode.this.getValue())).setInitialBindings((ArrayList)o);
                        }
            }, true));
            l.add(new ExpectationsNode(Messages.getString("LslTestContentProvider.CALL_EXPECTATIONS"), this,  //$NON-NLS-1$
                    ((LslTest)getValue()).getExpectations(), new ParentSetter() {
                        public void set(Object o) {
                            ((LslTest)getValue()).setExpectations((CallExpectations)o);
                        }
            }));
            l.add(new GlobalBindingsNode(Messages.getString("LslTestContentProvider.FINAL_GLOBALS"), this, test.getFinalBindings(), //$NON-NLS-1$
                    new ParentSetter() {
                        public void set(Object o) {
                            ((LslTest)(TestNode.this.getValue())).setFinalBindings((ArrayList)o);
                        }
            }, false));
            children = l;
		    
		}
		
		public String displayString() {
		    if (getValue() == null) return super.displayString();
		    LslTest test = (LslTest) getValue();
		    return test.name;
		}
		
		public Node[] getChildren() {
		    return (Node[]) children.toArray(new Node[children.size()]);
		}
		
		public boolean hasChildren() { return true; }
		
		public boolean isEditable() { return true; }
		
		public boolean isDeletable() { return true; }
		
		protected boolean setMyValue(Object o) {
		    return false;
		}
	}
	
	public class TextNode extends Node {
		public TextNode(String name, Node parent, Object value, ParentSetter setter) {
			super(name, parent, value, setter);
		}
		
		public String displayString() {
			if (getValue() == null) return super.displayString();
			return getValue().toString();
		}
		
		public Node[] getChildren() { return null; }
		
		public boolean hasChildren() { return false; }
		public boolean isEditable() { return true; }
		protected boolean setMyValue(Object o) {
			if (getValue().equals(o.toString())) return false;
			setValue(o.toString());
		    return true;
		}
	}
	
	private static final String BLANK = ""; //$NON-NLS-1$
	private TreeViewer fTreeViewer;
	private LslProjectNature nature;
	private String suiteName;
	private LslTestEditor testEditor;
	private SuiteNode topNode;
	
	public LslTestContentProvider(LslTestEditor testEditor, TreeViewer treeViewer, String name, 
	        LslProjectNature nature) {
		this.testEditor = testEditor;
		this.suiteName = name;
		this.nature = nature;
		this.fTreeViewer = treeViewer;
	}
	
	private LslValueNode createLslValueNode(String name, Node parent, Object value, Class type, 
	        boolean clearable, ParentSetter setter) {
	    if (value != null && !type.isAssignableFrom(value.getClass())) value = null; 
		if (type.equals(LslVector.class)) {
			return new LslVectorNode(name, parent, value, clearable, setter);
		} else if (type.equals(LslRotation.class)) {
			return new LslRotationNode(name, parent, value, clearable, setter);
		} else if (type.equals(LslString.class)) {
			return new LslStringNode(name, parent, value, clearable, setter);
		} else if (type.equals(LslKey.class)) {
			return new LslKeyNode(name, parent, value, clearable, setter);
		} else if (type.equals(LslInteger.class)) {
			return new LslIntegerNode(name, parent, value, clearable, setter);
		} else if (type.equals(LslFloat.class)) {
			return new LslFloatNode(name, parent,value,clearable, setter);
		} else if (type.equals(LslList.class)) {
			return new LslListNode(name, parent, value, clearable, setter);
		} else if (type.equals(LslVoid.class)) {
			return new LslVoidNode(name, parent, value, setter);
		} else if (type.equals(LslValue.class)) {
			return new LslUnknownValueNode(name, parent, setter);
		}
		
		return null;
	}
	
	public void dispose() {
	}
	
	public Object[] getChildren(Object element) { return ((Node)element).getChildren(); }
	
	public Object[] getElements(Object element) {
		if (testEditor.getTop() == null) return null;
		if (topNode == null) {
		    topNode = new SuiteNode(Messages.getString("LslTestContentProvider.SUITE"), null, testEditor.getTop()); //$NON-NLS-1$
		}
		
		return new Object[] { topNode };
	}
	
	public Object getParent(Object element) {
		return ((Node)element).parent;
	}
	
	public boolean hasChildren(Object element) {
		return ((Node)element).hasChildren();
	}
	
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}
	
	public boolean isDeleted(Object element) {
		return false;
	}
	
	public void refresh(Object element) {
		fTreeViewer.refresh(element);
	}

	private MaybeNode maybeNodeFactory(String name, Node parent, Object value, ParentSetter setter, Class type) {
	    if (value == null) { 
	        value = new MaybeValue();
	        setter.set(value);
	    }
	    
	    if (LslList.class.equals(type)) {
	        return new MaybeListNode(name, parent, value, setter);
	    } else {
	        return new MaybeNode(name, parent, value, setter, type);
	    }
	}

	private static LslFunction[] getFunctions() {
	    return LslPlusPlugin.getDefault().getLslMetaData().getFunctions();
	}
	
	private static LslFunction findFunction(String name) {
	    if (name == null) return null;
	    LslFunction[] functions = getFunctions();
	    
	    for (int i = 0; i < functions.length; i++) {
	        if (functions[i].getName().equals(name)) return functions[i];
	    }
	    
	    return null;
	}
}
