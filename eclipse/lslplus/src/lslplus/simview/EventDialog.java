package lslplus.simview;

import lslplus.LslPlusPlugin;
import lslplus.SimManager;
import lslplus.sim.SimEvent;
import lslplus.sim.SimEventArg;
import lslplus.sim.SimEventDefinition;
import lslplus.sim.SimParamDefinition;
import lslplus.sim.SimStatuses.NameKeyType;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

public class EventDialog extends Dialog {
    private Combo combo;
    private String[] args = null;
    private boolean[] argValid = null;
    private SimEventDefinition desc;
    protected EventDialog(Shell parentShell, SimEventDefinition desc) {
        super(parentShell);
        this.desc = desc;
        args = new String[desc.getParams().length];
        argValid  = new boolean[args.length];
        for (int i = 0; i < argValid.length; i++) argValid[i] = false;
    }

    private Button okButton() {
        return getButton(IDialogConstants.OK_ID);
    }
    
    private boolean selectionIsValid() {
        return combo.getSelectionIndex() >= 0;
    }

    private void updateButtons() {
        boolean valid = true;
        for (int i = 0; valid && i < args.length; i++) {
            valid = argValid[i];
        }
        
        okButton().setEnabled(valid);
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
        getShell().setText("Event: " + desc.getName());
        Composite  composite = (Composite) super.createDialogArea(parent);
        Label descriptionLabel = new Label(composite, SWT.LEAD|SWT.HORIZONTAL|SWT.WRAP);
        descriptionLabel.setText(desc.getDescription()); //$NON-NLS-1$
        
        SimParamDefinition[] parameters = desc.getParams();
        
        for (int i = 0; i < parameters.length; i++) {
            Composite comp = new Composite(parent, SWT.NONE);
            GridLayout layout = new GridLayout();
            layout.numColumns = 2;
//            layout.marginHeight = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_MARGIN);
//            layout.marginWidth = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_MARGIN);
//            layout.verticalSpacing = convertVerticalDLUsToPixels(IDialogConstants.VERTICAL_SPACING);
//            layout.horizontalSpacing = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_SPACING);
            comp.setLayout(layout);
            comp.setLayoutData(new GridData(GridData.FILL_BOTH));
            
            Label nameLabel = new Label(comp, SWT.LEAD|SWT.HORIZONTAL);
            nameLabel.setText(parameters[i].getName());
            nameLabel.setToolTipText(parameters[i].getControlID());
            
            String controlID = parameters[i].getControlID();
            if (controlID.startsWith("expression-")) {
                final Text text = new Text(comp, SWT.SINGLE|SWT.LEFT);
                final int argIndex = i;
                text.addModifyListener(new ModifyListener() {
                    public void modifyText(ModifyEvent e) {
                        args[argIndex] = text.getText();
                        argValid[argIndex] = text.getText().trim().length() > 0;
                        updateButtons();
                    }
                });
                
            } else if ("avatar".equals(controlID)) {
                createKeyCombo(i, comp, simManager().getSimState().getAvatars());
            } else if ("prim".equals(controlID)) {
                createKeyCombo(i, comp, simManager().getSimState().getPrims());
                
            }
        }
        
        return composite;
    }

    private void createKeyCombo(final int argIndex, Composite parent, NameKeyType[] nameKeyObj) {
        final Combo combo = new Combo(parent, SWT.READ_ONLY|SWT.DROP_DOWN);
        String[] dropdownRepresentation = new String[nameKeyObj.length];
        final String[] itemKey = new String[dropdownRepresentation.length];
        for (int j = 0; j < dropdownRepresentation.length; j++) {
            dropdownRepresentation[j] = nameKeyObj[j].getCombinedRepresentation();
            itemKey[j] = nameKeyObj[j].getKey();
        }
        combo.setItems(dropdownRepresentation);
        combo.deselectAll();
        
        combo.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            public void widgetSelected(SelectionEvent e) {
                if (combo.getSelectionIndex() >= 0) {
                    argValid[argIndex] = true;
                    args[argIndex] = itemKey[combo.getSelectionIndex()];
                } else {
                    argValid[argIndex] = false;
                    args[argIndex] = null;
                }
                updateButtons();
            }
        });
    }

    private SimManager simManager() {
        return LslPlusPlugin.getDefault().getSimManager();
    }

    public SimEvent getEvent() {
        if (this.getReturnCode() != Dialog.OK) return null;
        SimEventArg[] simEventArgs = new SimEventArg[args.length];
        for (int i = 0; i < simEventArgs.length; i++) {
            simEventArgs[i] = new SimEventArg(desc.getParams()[i].getName(), args[i]);
        }
        return new SimEvent(desc.getName(), 0, simEventArgs);
    }
}
