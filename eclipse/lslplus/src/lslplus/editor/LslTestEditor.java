package lslplus.editor;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import lslplus.LslPlusPlugin;
import lslplus.LslProjectNature;
import lslplus.LslProjectNature.NameTypePair;
import lslplus.editor.LslTestContentProvider.CallListNode;
import lslplus.editor.LslTestContentProvider.ChoiceProvider;
import lslplus.editor.LslTestContentProvider.Emptyable;
import lslplus.editor.LslTestContentProvider.EntryPointNode;
import lslplus.editor.LslTestContentProvider.GlobalBindingsNode;
import lslplus.editor.LslTestContentProvider.LslValueAddable;
import lslplus.editor.LslTestContentProvider.Node;
import lslplus.editor.LslTestContentProvider.SuiteNode;
import lslplus.editor.LslTestContentProvider.TestNode;
import lslplus.language_metadata.LslFunction;
import lslplus.language_metadata.LslParam;
import lslplus.lsltest.LslTest;
import lslplus.lsltest.LslTestSuite;
import lslplus.lsltest.LslTest.EntryPoint;
import lslplus.lsltest.LslTest.ExpectedCall;
import lslplus.lsltest.LslTest.GlobBinding;
import lslplus.lsltest.LslTest.LslFloat;
import lslplus.lsltest.LslTest.LslInteger;
import lslplus.lsltest.LslTest.LslKey;
import lslplus.lsltest.LslTest.LslRotation;
import lslplus.lsltest.LslTest.LslString;
import lslplus.lsltest.LslTest.LslValue;
import lslplus.lsltest.LslTest.LslVector;
import lslplus.util.Util;
import lslplus.util.Util.ArrayMapFunc;
import lslplus.util.Util.Predicate;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CellLabelProvider;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.ComboBoxCellEditor;
import org.eclipse.jface.viewers.DialogCellEditor;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.ICellEditorValidator;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TextCellEditor;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.TreeViewerColumn;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.EditorPart;

public class LslTestEditor extends EditorPart {
	
	private final class LslTestEditingSupport extends EditingSupport {
	    private CellEditor curEditor = null;
	    private Shell shell;
		private TextCellEditor textEditor = new TextCellEditor(fTree);
		private LslTestEditingSupport(ColumnViewer viewer, Shell shell) {
			super(viewer);
			this.shell = shell;
		}

		protected boolean canEdit(Object element) {
			return ((Node) element).isEditable();
		}

		protected CellEditor getCellEditor(Object element) {
			final Node n = (Node) element;
			
			if (n instanceof ChoiceProvider) {
				ChoiceProvider p = (ChoiceProvider)n;
				curEditor = new ComboBoxCellEditor(fTree,p.getChoices());
				return curEditor;
			} else if (n instanceof EntryPointNode) {
			    curEditor = new EntryPointCellEditor(fTree, (EntryPoint)n.getValue());
			    return curEditor;
			}
			
			textEditor.setValidator(new ICellEditorValidator() {
				public String isValid(Object value) {
					return n.isValid(value);
				}
				
			});
			curEditor = textEditor;
			return textEditor;
		}

		protected Object getValue(Object element) {
			Node n = (Node) element;
			if (n instanceof ChoiceProvider) {
				if (n.getValue() == null || n.getValue().toString().trim().equals(BLANK))  return new Integer(0);
				
				String[] choices = ((ChoiceProvider)n).getChoices();
				
				int result = 0;
				
				for (int i = 0; i < choices.length; i++) {
					if (choices[i].equals(n.getValue())) {
						result = i;
						break;
					}
				}
				
				return new Integer(result);
			} else {
				if (n == null) return BLANK;
				else return n.getEditableValue();
			}
		}

		protected void setValue(Object element, Object value) {
			Node n = (Node) element;
			
			// TODO: fix hack for showing error
			if (curEditor == null) return;
			if (!curEditor.isValueValid()) {
			    String message = curEditor.getErrorMessage();
                curEditor = null; // this is part of the hack ...
                                  // when the message dialog is displayed, focus to the editor
                                  // is lost, triggering a 2nd (concurrent) setValue call...
                                  // we supress this by nulling the editor reference, and checking
                                  // it above...
			    MessageDialog.openError(shell, Messages.getString("LslTestEditor.VALUE_ERROR"), message); //$NON-NLS-1$
			    return;
			}
			if (n instanceof TestNode) {
			    LslTest test = (LslTest)n.getValue();
			    test.name = (String) value;
			    fTreeViewer.update(element, null);
			    setDirty(true);
			    return;
			}
			if (n.set(value)) setDirty(true);
			fTreeViewer.update(element, null);
			fTreeViewer.update(n.getParent(), null);
		}
	}
	private AddTestAction fAddTestAction;
	private AddCallAction1 fAddCallAction;
	private TreeColumn fColumn1;
	private TreeColumn fColumn2;
	private TreeViewer fTreeViewer;
	private Tree fTree;
	private LslTestLabelProvider fLabelProvider;
	private LslTestSuite suite = null;
	private String suiteName = null;
	private LslProjectNature nature = null;
	private boolean dirty;
	private IFile file;
	private AddListItemAction fAddStringItemAction;
	private AddListItemAction fAddKeyItemAction;
	private AddListItemAction fAddIntegerItemAction;
	private AddListItemAction fAddFloatItemAction;
	private AddListItemAction fAddVectorItemAction;
	private AddListItemAction fAddRotationItemAction;
	private ClearValueAction fClearValueAction;
	private DeleteNodeAction fDeleteNodeAction;
    private EmptyContentsAction fEmptyListAction;
    private AddGlobalAction fAddInitialGlobalAction;
    private AddGlobalAction fAddFinalGlobalAction;
	private static final String BLANK = ""; //$NON-NLS-1$
	private static String[] statefulFunctions = null;
	public LslTestEditor() {
	}

	public void doSave(IProgressMonitor monitor) {
		String val = suite.toXml();
		if (LslPlusPlugin.DEBUG) Util.log("suite = " + val); //$NON-NLS-1$
		try {
			file.setContents(new ByteArrayInputStream(val.getBytes()), IResource.FORCE | IResource.KEEP_HISTORY, monitor);
			setDirty(false);
		} catch (CoreException e) {
			Util.log(e, e.getLocalizedMessage());
		}
	}

	public void doSaveAs() {
	}

	public void init(IEditorSite site, IEditorInput input)
			throws PartInitException {
		this.setSite(site);
		this.setInput(input);
		
		this.setPartName(input.getName());
		file = (IFile) input.getAdapter(IFile.class);
		try {
			nature = (LslProjectNature) file.getProject().getNature(LslProjectNature.ID);
		} catch (CoreException e1) {
			throw new PartInitException(Messages.getString("LslTestEditor.CANT_GET_PROJECT_NATURE"), e1); //$NON-NLS-1$
		}
		IPath fullPath = file.getFullPath();
		suiteName = fullPath.removeFileExtension().lastSegment();
		if (file != null) {
			try {
				suite = LslTestSuite.fromXml(file.getContents(), file);
			} catch (CoreException e) {
				Util.log(e, Messages.getString("LslTestEditor.CORRUPTED_TEST_FILE") + e.getMessage()); //$NON-NLS-1$
				suite = null;
			}
		}
	}

	public boolean isDirty() {
		return dirty;
	}

	public boolean isSaveAsAllowed() {
		return false;
	}

	public void createPartControl(Composite parent) {
		//control = new Composite(parent, SWT.NULL);
		//control.setVisible(true);
		createViewer(parent);
	}

	public void setFocus() {
	}

	private void createViewer(Composite parent) {
		fTreeViewer = new TreeViewer(parent, SWT.SINGLE | SWT.FULL_SELECTION);
		fTree = fTreeViewer.getTree();
		fTree.setLinesVisible(true);
		createColumns(fTree);
		fTreeViewer.setContentProvider(new LslTestContentProvider(this, fTreeViewer, suiteName, nature));
		fTreeViewer.setLabelProvider(fLabelProvider = new LslTestLabelProvider());
		fLabelProvider.connect(this);
		fTreeViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			public void selectionChanged(SelectionChangedEvent e) {
				TreeSelection selection = (TreeSelection) e.getSelection();
				
				if (fAddTestAction != null) {
					Node n = (Node) selection.getFirstElement();
					fAddTestAction.setEnabled(n == null || 
							n instanceof SuiteNode);
				}
			}
		});
		fTreeViewer.addDoubleClickListener(new IDoubleClickListener() {
			public void doubleClick(DoubleClickEvent event) {
			}		
		});
		fTreeViewer.setInput(this);
		
		
		TreeViewerColumn column2 = new TreeViewerColumn(fTreeViewer, fColumn2);
		column2.setLabelProvider(new CellLabelProvider() {
			public void update(ViewerCell cell) {
				if (cell.getColumnIndex() == 0) {
					cell.setImage(LslTestLabelProvider.dummy());
				}
				Node n = (Node) cell.getElement();
				if (n == null) cell.setText(BLANK);
				else cell.setText(n.displayString());
			}
		});
		
		column2.setEditingSupport(new LslTestEditingSupport(fTreeViewer, getShell()));
		addMouseListeners();
		fAddTestAction = new AddTestAction();
		fAddCallAction = new AddCallAction1();
		fAddStringItemAction = new AddListItemAction(Messages.getString("LslTestEditor.Add_string")) { //$NON-NLS-1$
			public LslValue newItem() {
				return LslTest.defaultValueFor(LslString.class);
			}
			
		};
		fAddKeyItemAction = new AddListItemAction(Messages.getString("LslTestEditor.Add_key")) { //$NON-NLS-1$
			public LslValue newItem() {
				return LslTest.defaultValueFor(LslKey.class);
			}
			
		};
		fAddIntegerItemAction = new AddListItemAction(Messages.getString("LslTestEditor.Add_integer")) { //$NON-NLS-1$
			public LslValue newItem() {
				return LslTest.defaultValueFor(LslInteger.class);
			}
			
		};
		fAddFloatItemAction = new AddListItemAction(Messages.getString("LslTestEditor.Add_float")) { //$NON-NLS-1$
			public LslValue newItem() {
				return LslTest.defaultValueFor(LslFloat.class);
			}
			
		};
		fAddVectorItemAction = new AddListItemAction(Messages.getString("LslTestEditor.Add_vector")) { //$NON-NLS-1$
			public LslValue newItem() {
				return LslTest.defaultValueFor(LslVector.class);
			}
			
		};
		fAddRotationItemAction = new AddListItemAction(Messages.getString("LslTestEditor.Add_rotation")) { //$NON-NLS-1$
			public LslValue newItem() {
				return LslTest.defaultValueFor(LslRotation.class);
			}
			
		};
		
		fClearValueAction = new ClearValueAction();
		fDeleteNodeAction = new DeleteNodeAction();
		fEmptyListAction = new EmptyContentsAction();
		fAddInitialGlobalAction = new AddGlobalAction(true);
        fAddFinalGlobalAction = new AddGlobalAction(false);
		
		MenuManager popupMenuManager = new MenuManager("#PopupMenu"); //$NON-NLS-1$
		IMenuListener listener = new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				Node n = getSelectedNode();
				if (n == null || n instanceof SuiteNode) manager.add(fAddTestAction);
				//manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
				if (n instanceof LslValueAddable) {
					manager.add(fAddFloatItemAction);
					manager.add(fAddIntegerItemAction);
					manager.add(fAddKeyItemAction);
					manager.add(fAddRotationItemAction);
					manager.add(fAddStringItemAction);
					manager.add(fAddVectorItemAction);
					manager.add(new Separator());
				}
				
				if (n instanceof CallListNode) {
				    manager.add(fAddCallAction);
				}
				if (n instanceof GlobalBindingsNode) {
				    GlobalBindingsNode gbn = (GlobalBindingsNode) n;
				    
				    LslTest test = (LslTest) n.getParent().getValue();
				    if (nature.getGlobalVariables(test.getEntryPoint().getFileName()).length > 0) {
				        if (gbn.isInitial()) {
				            fAddInitialGlobalAction.setEnabled(
				                !determineRemainingGlobals(test.getEntryPoint().getFileName(),
				                                           test.getInitialBindings()).isEmpty());
				            manager.add(fAddInitialGlobalAction);
				        } else {
				            fAddFinalGlobalAction.setEnabled(
				                !determineRemainingGlobals(test.getEntryPoint().getFileName(),
				                    test.getFinalBindings()).isEmpty());
				            manager.add(fAddFinalGlobalAction);
				        }
				    }
				}
				
				if (n != null && n.isClearable()) manager.add(fClearValueAction);
				if (n != null && n.isDeletable()) manager.add(fDeleteNodeAction);
				if (n instanceof Emptyable &&
				        !((Emptyable)n).alreadyEmpty()) manager.add(fEmptyListAction);
			}
		};
		popupMenuManager.addMenuListener(listener);
		popupMenuManager.setRemoveAllWhenShown(true);
		//getSite().registerContextMenu(popupMenuManager, getSite().getSelectionProvider());
		Menu menu = popupMenuManager.createContextMenu(fTree);
		fTree.setMenu(menu);

	}

	private Node getSelectedNode() {
		ITreeSelection selection = (ITreeSelection) fTreeViewer.getSelection();
		return (selection!=null) ? (Node) selection.getFirstElement() : null;
		
	}
	
    private HashMap determineRemainingGlobals(String fileName, List globs) {
        NameTypePair[] pairs = nature.getGlobalVariables(fileName);
        Set used = Util.mapToSet(new ArrayMapFunc() {
            public Class elementType() { return String.class; }
            public Object map(Object o) { return ((GlobBinding)o).getName(); }
        }, globs);
        HashMap map = new HashMap();
        for (int i = 0; i < pairs.length; i++) {
            if (!used.contains(pairs[i].getName())) {
                map.put(pairs[i].getName(), pairs[i]);
            }
        }
        return map;
    }
	
	private abstract class AddListItemAction extends Action {
		public AddListItemAction(String message) {
			super(message);
		}
		
		public void run() {
			Node n = getSelectedNode();
			if (n == null) return;
			if (n instanceof LslValueAddable) {
			    LslValueAddable addable = (LslValueAddable) n;
			    setDirty(true);
			    addable.add(newItem());
			    fTreeViewer.refresh(n);
			}
		}
		
		public abstract LslValue newItem();
	}
	
	private class AddTestAction extends Action {
	    public AddTestAction() {
	        super(Messages.getString("LslTestEditor.ADD_TEST")); //$NON-NLS-1$
	    }
	    
	    public void run() {
            Node n = getSelectedNode();
            if (n instanceof SuiteNode) {
                SuiteNode sn = (SuiteNode) n;
    	        EntryPointSelectionDialog d = new EntryPointSelectionDialog(getShell(), null, null);
    	        
    	        d.open();
    	        
    	        if (d.getReturnCode() == Window.OK) {
                    LslTestEditor.this.setDirty(true);
                    LslTestSuite suite = (LslTestSuite) n.getValue();
                    LslTest t = new LslTest();
                    t.setSuite(suite);
                    EntryPoint ep = new EntryPoint();
                    ep.setFileName(d.getFilename());
                    ep.setPath(d.getPath());
                    t.setEntryPoint(ep);
                    sn.addTest(t);
                    fTreeViewer.refresh();
    	        }
            }
	    }
	}
	
	private class ClearValueAction extends Action {
		public ClearValueAction() {
			super(Messages.getString("LslTestEditor.Clear_value")); //$NON-NLS-1$
		}
		
		public void run() {
			Node n = getSelectedNode();
			
			if (n != null && n.isClearable()) {
				n.clear();
				fTreeViewer.refresh(n);
				setDirty(true);
			}
		}
	}
	
	private class DeleteNodeAction extends Action {
		public DeleteNodeAction() {
			super(Messages.getString("LslTestEditor.Delete")); //$NON-NLS-1$
		}
		
		public void run() {
			Node n = getSelectedNode();
			if (n != null && n.getParent() != null && n.isDeletable()) {
			    Node parent = n.getParent();
				parent.deleteChild(n);
				setDirty(true);
				fTreeViewer.refresh(parent);
			}
		}
	}
	
    private class EmptyContentsAction extends Action {
        public EmptyContentsAction() {
            super(Messages.getString("LslTestEditor.EMPTY_LIST")); //$NON-NLS-1$
        }
        
        public void run() {
            Node n = getSelectedNode();
            if (n != null && n instanceof Emptyable) {
                Emptyable em = (Emptyable) n;
                em.emptyContents();
                setDirty(true);
                fTreeViewer.refresh(n);
            }
        }
    }

    private class AddCallAction1 extends Action {
        public AddCallAction1() {
            super(Messages.getString("LslTestEditor.ADD_CALL")); //$NON-NLS-1$
        }
        
        public void run() {
            Node n = getSelectedNode();
            if (n instanceof CallListNode) {
                CallSelectionDialog dlg = new CallSelectionDialog(getShell(), null);
                dlg.open();
                
                if (dlg.getReturnCode() == Window.OK) {
                    final ExpectedCall call = new ExpectedCall();
                    final String name = dlg.getName();
                    LslFunction func = (LslFunction) Util.find(new Predicate() {
                        public boolean test(Object o) {
                            return name.equals(((LslFunction)o).getName());
                        }
                    }, getFunctions());
                    
                    call.setName(name);
                    call.setReturns(
                            LslTest.defaultValueFor(LslTest.stringToLslType(func.getReturns())));
                    ArrayList l = new ArrayList();
                    
                    LslParam[] params = func.getParams();
                    for (int i = 0; i < params.length; i++) {
                        l.add(new LslTest.MaybeValue());
                    }
                    call.setArgs(l);
                    setDirty(true);
                    ((CallListNode)n).addChild(call);
                    fTreeViewer.refresh(n);
                }
            }
        }
    }
    
    private class AddGlobalAction extends Action {
        private boolean initial;
        public AddGlobalAction(boolean initial) {
            super(Messages.getString("LslTestEditor.ADD_GLOBAL")); //$NON-NLS-1$
            this.initial = initial;
        }
        
        public void run() {
            Node n = getSelectedNode();
            if (n instanceof GlobalBindingsNode) {
                TestNode tn = (TestNode) n.getParent();
                LslTest test = (LslTest) tn.getValue();
                String fileName = test.getEntryPoint().getFileName();
                GlobalSelectionDialog dlg = new GlobalSelectionDialog(getShell(), fileName, 
                        initial ? test.getInitialBindings() : test.getFinalBindings());
                dlg.open();
                
                if (dlg.getReturnCode() == Window.OK) {
                    final GlobBinding binding = new GlobBinding();
                    final NameTypePair pair = dlg.getPair();
                    binding.setName(pair.getName());
                    binding.setValue(
                            LslTest.defaultValueFor(LslTest.stringToLslType(pair.getType())));

                    setDirty(true);
                    ((GlobalBindingsNode)n).addChild(binding); 
                    fTreeViewer.refresh(n);
                }
            }
        }
    }
    
	private void createColumns(Tree tree) {
		fColumn1 = new TreeColumn(tree, SWT.LEFT);
		fColumn1.setText(Messages.getString("LslTestEditor.ITEM"));  //$NON-NLS-1$
		fColumn1.setWidth(200);
		fColumn1.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
			}
		});

		fColumn2 = new TreeColumn(tree, SWT.LEFT);
		fColumn2.setText(Messages.getString("LslTestEditor.VALUE"));  //$NON-NLS-1$
		fColumn2.setWidth(100);
		fColumn2.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
			}
		});

		tree.setHeaderVisible(true);
	}

	public void setDirty(boolean b) {
		dirty = b;
		firePropertyChange(PROP_DIRTY);
	}

	public Object getTop() {
		return suite;
	}
	private void addMouseListeners() {
		Listener tableListener = new Listener() {
			public void handleEvent(Event e) {
				switch (e.type) {
				case SWT.MouseMove:
					break;
				case SWT.MouseHover:
					break;
				case SWT.MouseDown:
					break;
				}
			}
		};
		int[] tableEvents = new int[] { SWT.MouseDown, SWT.MouseMove, SWT.MouseHover };
		for (int i = 0; i < tableEvents.length; i++) {
			fTree.addListener(tableEvents[i], tableListener);
		}
	}
	
	public void fillContextMenu(IMenuManager manager) {
	}

	private Shell getShell() {
        return LslTestEditor.this.getEditorSite().getShell();
    }

    private class CallSelectionDialog extends Dialog {
        private String name;
        private Combo combo;
        protected CallSelectionDialog(Shell parentShell, String name) {
            super(parentShell);
            this.name = name;
        }
        
        private Button okButton() {
            return getButton(IDialogConstants.OK_ID);
        }
        
        private boolean selectionIsValid() {
            return combo.getSelectionIndex() >= 0;
        }

        protected Control createButtonBar(Composite parent) {
            Control c = super.createButtonBar(parent);
            
            okButton().addListener(SWT.Show, new Listener() {
               public void handleEvent(Event event) {
                   okButton().setEnabled(selectionIsValid());
               }
            });
            return c;
        }
        
        protected Control createDialogArea(Composite parent) {
            getShell().setText(Messages.getString("LslTestEditor.ENTER_EXPECTED_FUNCTION_CALL")); //$NON-NLS-1$
            Composite  composite = (Composite) super.createDialogArea(parent);
            Label nameLabel = new Label(composite, SWT.LEFT|SWT.HORIZONTAL);
            nameLabel.setText(Messages.getString("LslTestEditor.FUNCTION_NAME")); //$NON-NLS-1$
            
            combo = new Combo(composite, SWT.READ_ONLY|SWT.DROP_DOWN);

             String[] items = getStatefulFunctions();
            int index = Util.elementIndex(name, items);
            
            combo.setItems(items);
            if (index >= 0) combo.select(index);
            else combo.deselectAll();
            
            combo.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent e) {
                }

                public void widgetSelected(SelectionEvent e) {
                    if (combo.getSelectionIndex() >= 0) {
                        name = combo.getItem(combo.getSelectionIndex());
                        okButton().setEnabled(true);
                    } else {
                        okButton().setEnabled(false);
                    }
                }
            });
            
            return composite;
        }

        public String getName() { return name; }
	}
	
    private class GlobalSelectionDialog extends Dialog {
        private NameTypePair pair;
        private Combo combo;
        private HashMap pairsMap = new HashMap();
        protected GlobalSelectionDialog(Shell parentShell, String fileName, List globs) {
            super(parentShell);
            
            pairsMap = determineRemainingGlobals(fileName, globs);
        }

        private Button okButton() {
            return getButton(IDialogConstants.OK_ID);
        }
        
        private boolean selectionIsValid() {
            return combo.getSelectionIndex() >= 0;
        }

        protected Control createButtonBar(Composite parent) {
            Control c = super.createButtonBar(parent);
            okButton().setEnabled(false);
            okButton().addListener(SWT.Show, new Listener() {
               public void handleEvent(Event event) {
                   okButton().setEnabled(selectionIsValid());
               }
            });
            return c;
        }
        
        protected Control createDialogArea(Composite parent) {
            getShell().setText(Messages.getString("LslTestEditor.SELECT_GLOBAL")); //$NON-NLS-1$
            Composite  composite = (Composite) super.createDialogArea(parent);
            Label nameLabel = new Label(composite, SWT.LEFT|SWT.HORIZONTAL);
            nameLabel.setText(Messages.getString("LslTestEditor.VARIABLE_NAME")); //$NON-NLS-1$
            
            combo = new Combo(composite, SWT.READ_ONLY|SWT.DROP_DOWN);

            ArrayList l = new ArrayList();
            l.addAll(pairsMap.keySet());
            Collections.sort(l);
            final String[] items = (String[]) l.toArray(new String[l.size()]);
            combo.setItems(items);
            combo.deselectAll();
            
            combo.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent e) {
                }

                public void widgetSelected(SelectionEvent e) {
                    if (combo.getSelectionIndex() >= 0) {
                        String name = items[combo.getSelectionIndex()];
                        pair = (NameTypePair) pairsMap.get(name);
                        okButton().setEnabled(true);
                    } else {
                        okButton().setEnabled(false);
                    }
                }
            });
            
            return composite;
        }

        
        public NameTypePair getPair() { return pair; }
    }
    
    private class EntryPointSelectionDialog extends Dialog {
        private String fileName;
        private String path;
        private Combo combo;
        private Combo combo2;
        protected EntryPointSelectionDialog(Shell parentShell, String fileName, String entryPoint) {
            super(parentShell);
            this.fileName = fileName;
            this.path = entryPoint;
        }

        private Button okButton() {
            return getButton(IDialogConstants.OK_ID);
        }
        
        protected Control createButtonBar(Composite parent) {
             Control c = super.createButtonBar(parent);
             
             okButton().addListener(SWT.Show, new Listener() {
                public void handleEvent(Event event) {
                    okButton().setEnabled(selectionIsValid());
                }
             });
             return c;
        }

        private boolean selectionIsValid() {
            return combo.getSelectionIndex() >= 0 && combo2.getSelectionIndex() >= 0;
        }
        
        protected Control createDialogArea(Composite parent) {
            getShell().setText(Messages.getString("LslTestEditor.ENTER_TEST_ENTRY_POINT")); //$NON-NLS-1$
            Composite  composite = (Composite) super.createDialogArea(parent);
            GridLayout layout = (GridLayout) composite.getLayout();
            layout.numColumns = 2;
            Label fileNameLabel = new Label(composite, SWT.LEFT|SWT.HORIZONTAL);
            fileNameLabel.setText(Messages.getString("LslTestEditor.FILE_NAME")); //$NON-NLS-1$
            Label pathLabel = new Label(composite, SWT.LEFT|SWT.HORIZONTAL);
            pathLabel.setText(Messages.getString("LslTestEditor.ENTRY_POINT")); //$NON-NLS-1$
            
            combo = new Combo(composite, SWT.READ_ONLY|SWT.DROP_DOWN);
            combo2 = new Combo(composite, SWT.READ_ONLY|SWT.DROP_DOWN);
            
            String[] files = nature.getLslFiles();
            int fIndex = Util.elementIndex(fileName, files);
            
            combo.setItems(files);
            if (fIndex >= 0) combo.select(fIndex);
            else combo.deselectAll();
            
            int eIndex = -1;
            if (fIndex >= 0) {
                String[] eps = nature.getEntryPointNames(fileName);
                eIndex = Util.elementIndex(path, eps);
                combo2.setItems(eps);
                combo2.select(eIndex);
            }
            combo2.setEnabled(eIndex >= 0);
            
            combo.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent e) {
                }

                public void widgetSelected(SelectionEvent e) {
                    if (combo.getSelectionIndex() >= 0) {
                        String fileName = combo.getItem(combo.getSelectionIndex());
                        combo2.setItems(nature.getEntryPointNames(fileName));
                        combo2.deselectAll();
                        combo2.setEnabled(true);
                        EntryPointSelectionDialog.this.fileName = fileName;
                        path = null;
                        okButton().setEnabled(false);
                    } else {
                        EntryPointSelectionDialog.this.fileName = null;
                        combo2.setEnabled(false);
                        okButton().setEnabled(false);
                    }
                    if (LslPlusPlugin.DEBUG) Util.log("fileName = " + fileName + ", path = " + path); //$NON-NLS-1$ //$NON-NLS-2$
                }
            });
            
            combo2.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent e) {
                    okButton().setEnabled(combo2.getSelectionIndex() >= 0);
                }

                public void widgetSelected(SelectionEvent e) {
                    okButton().setEnabled(combo2.getSelectionIndex() >= 0);
                    
                    if (combo2.getSelectionIndex() >= 0) {
                        path = combo2.getItem(combo2.getSelectionIndex());
                    } else {
                        path = null;
                    }
                    if (LslPlusPlugin.DEBUG) Util.log("path = " + path); //$NON-NLS-1$
                }
            });
            return composite;
        }
        
        public String getFilename() {
            return fileName;
        }
        
        public String getPath() {
            return path;
        }
    }
    
    private class EntryPointCellEditor extends DialogCellEditor {
        private EntryPoint ep;
        public EntryPointCellEditor(Composite c, EntryPoint ep) {
            super(c);
            this.ep = ep;
        }
        
        protected Object openDialogBox(Control cellEditorWindow) {
            EntryPointSelectionDialog dlg = 
                new EntryPointSelectionDialog(cellEditorWindow.getShell(), ep.getFileName(),
                        ep.getPath());
            
            dlg.open();
            
            if (dlg.getReturnCode() == Window.OK) {
                EntryPoint ep1 = new EntryPoint();
                ep1.setFileName(dlg.getFilename());
                ep1.setPath(dlg.getPath());
                return ep1;
            }
            
            return null;
        }

        protected void updateContents(Object value) {
            EntryPoint ep = (EntryPoint) value;
            if (value == null) this.getDefaultLabel().setText(BLANK);
            else this.getDefaultLabel().setText(ep.getFileName() + "/" + ep.getPath()); //$NON-NLS-1$
        }
    }

    private static LslFunction[] getFunctions() {
        return LslPlusPlugin.getDefault().getLslMetaData().getFunctions();
    }
    
    public static synchronized String[] getStatefulFunctions() {
        if (statefulFunctions == null) {
            List funcs = Util.filtMap(new ArrayMapFunc() {
                public Class elementType() { return String.class; }
                public Object map(Object o) {
                    LslFunction f = (LslFunction) o;
                    return f.isStateless() ? null : f.getName();
                }
            }, getFunctions());
            
            statefulFunctions = (String[]) funcs.toArray(new String[funcs.size()]);
        }
        
        return statefulFunctions;
    }
}
