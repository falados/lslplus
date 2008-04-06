package lslplus.simview;

import lslplus.LslPlusPlugin;
import lslplus.SimListener;
import lslplus.SimManager;
import lslplus.sim.SimStatuses;
import lslplus.simview.UserEventDescription.ParameterDescription;
import lslplus.simview.UserEventDescription.PrimParameter;
import lslplus.util.Util;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ViewForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.part.ViewPart;

public class SimWatcherViewPart extends ViewPart implements SimListener {
    public static final String ID = "lslplus.simWatcher"; //$NON-NLS-1$
    private static UserEventDescription touchEventDescription =
        new UserEventDescription("Touch Event", "Touch a prim",
                new ParameterDescription[] {
                    new PrimParameter()
                });
    private static class TouchDialog extends Dialog {
        private Combo combo;
        protected TouchDialog(Shell parentShell) {
            super(parentShell);
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
            getShell().setText("Select a prim to touch");
            Composite  composite = (Composite) super.createDialogArea(parent);
            Label nameLabel = new Label(composite, SWT.LEFT|SWT.HORIZONTAL);
            nameLabel.setText("Prim Name / Key"); //$NON-NLS-1$
            
            combo = new Combo(composite, SWT.READ_ONLY|SWT.DROP_DOWN);

            combo.setItems(new String[] { "foo", "bar" });
            combo.deselectAll();
            
            combo.addSelectionListener(new SelectionListener() {
                public void widgetDefaultSelected(SelectionEvent e) {
                }

                public void widgetSelected(SelectionEvent e) {
                    if (combo.getSelectionIndex() >= 0) {
                        okButton().setEnabled(true);
                    } else {
                        okButton().setEnabled(false);
                    }
                }
            });
            
            return composite;
        }
    }
    private class StopAction extends Action {
        private Shell parent;
        public StopAction(Shell parentShell) {
            this.parent = parentShell;
            setText("Stop Sim"); //$NON-NLS-1$
            setToolTipText("Stop Sim"); //$NON-NLS-1$
            ImageDescriptor descriptor = LslPlusPlugin
                    .imageDescriptorFromPlugin("icons/stop.gif"); //$NON-NLS-1$
            setHoverImageDescriptor(descriptor);
            setImageDescriptor(descriptor);
            setEnabled(false);
        }

        public void run() {
            getSimManager().stopSim();
        }
    }

    private class TouchAction extends Action {
        private Shell parentShell;
        public TouchAction(Shell parentShell) {
            this.parentShell = parentShell;
            setText("Touch"); //$NON-NLS-1$
            setToolTipText("Touch!"); //$NON-NLS-1$
            ImageDescriptor descriptor = LslPlusPlugin
                    .imageDescriptorFromPlugin("icons/touch.gif"); //$NON-NLS-1$
            setHoverImageDescriptor(descriptor);
            setImageDescriptor(descriptor);
            setEnabled(false);
        }

        public void run() {
            Dialog dlg = //new TouchDialog(parentShell);
                    new EventDialog(parentShell, touchEventDescription);
            dlg.open();
            
            if (dlg.getReturnCode() == Window.OK) {
                Util.log("OK!");
            }
        }
    }

    private Composite parent;
    private Composite counterComposite;
    private SashForm sashForm;
    private TreeViewer logViewer;
    private SimManager simManager;

    private LogViewerContentProvider logViewerModel;
    private TreeColumn fColumn1;
    private TreeColumn fColumn2;


    private Tree logViewerTree;
    private TouchAction touchAction;
    private StopAction stopAction;
    private TreeColumn fColumn3;
    private LogViewerLabelProvider labelProvider;

    public SimWatcherViewPart() {
        simManager = LslPlusPlugin.getDefault().getSimManager();
        simManager.addSimListener(this);
    }

    public void dispose() {
        super.dispose();
        simManager.removeSimListener(this);
        if (labelProvider != null) labelProvider.dispose();
    }

    public boolean isCreated() {
        return counterComposite != null;
    }

    public void createPartControl(Composite parent) {
        this.parent = parent;
        GridLayout gridLayout = new GridLayout();
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        parent.setLayout(gridLayout);
        SashForm sashForm = createSashForm(parent);
        sashForm.setLayoutData(new GridData(GridData.FILL_BOTH));
        configureToolBar();
    }

    private SashForm createSashForm(Composite parent2) {
        sashForm = new SashForm(parent, SWT.VERTICAL);

        ViewForm top = new ViewForm(sashForm, SWT.NONE);

        Composite empty = new Composite(top, SWT.NONE);
        empty.setLayout(new Layout() {
            protected Point computeSize(Composite composite, int wHint, int hHint,
                    boolean flushCache) {
                return new Point(1, 1);
            }

            protected void layout(Composite composite, boolean flushCache) {
            }
        });
        top.setTopLeft(empty);
        logViewer = createLogViewer(top);
        top.setContent(logViewer.getControl());

        return sashForm;
    }

    private TreeViewer createLogViewer(Composite parent) {
        logViewer = new TreeViewer(parent, SWT.SINGLE);
        logViewerTree = logViewer.getTree();
        createColumns(logViewerTree);
        logViewerModel = new LogViewerContentProvider();
        logViewer.setContentProvider(logViewerModel);
        labelProvider = new LogViewerLabelProvider();
        logViewer.setLabelProvider(labelProvider);
        logViewer.setInput(this);
        return logViewer;
    }
    
    private void createColumns(Tree tree) {
        fColumn1 = new TreeColumn(tree, SWT.LEFT);
        fColumn1.setText("Time");  //$NON-NLS-1$
        fColumn1.setWidth(50);
        fColumn1.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
            }
        });

        fColumn2 = new TreeColumn(tree, SWT.LEFT);
        fColumn2.setText("Source");  //$NON-NLS-1$
        fColumn2.setWidth(200);
        fColumn2.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
            }
        });
        
        fColumn3 = new TreeColumn(tree, SWT.LEFT);
        fColumn3.setText("Message");  //$NON-NLS-1$
        fColumn3.setWidth(300);
        fColumn3.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
            }
        });

        tree.setHeaderVisible(true);
    }

    public void setFocus() {
    }

    private void configureToolBar() {
        IActionBars actionBars = getViewSite().getActionBars();
        IToolBarManager toolBar = actionBars.getToolBarManager();
        
        touchAction = new TouchAction(this.getSite().getShell());
        stopAction = new StopAction(this.getSite().getShell());
        touchAction.setEnabled(simManager.isSimActive());
        stopAction.setEnabled(simManager.isSimActive());
        toolBar.add(touchAction);
        toolBar.add(stopAction);

        toolBar.add(new Separator());

        actionBars.updateActionBars();
    }

    public void newLogMessages(SimStatuses.Message[] result) {
        this.logViewerModel.addMessages(result);
        refreshAsync();
    }

    public void simLaunched() {
        touchAction.setEnabled(true);
        stopAction.setEnabled(true);
        if (logViewerModel != null) {
            this.logViewerModel.clear();
            refreshAsync();
        }
    }

    private Runnable refresher = new Runnable() {
        public void run() {
            logViewer.refresh();
        }
    };

    private void refreshAsync() {
        LslPlusPlugin.getDefault().getWorkbench().getDisplay().asyncExec(refresher);
    }
    
    private SimManager getSimManager() {
        return simManager;
    }

    public void simEnded() {
        touchAction.setEnabled(false);
        stopAction.setEnabled(false);
    }
}
