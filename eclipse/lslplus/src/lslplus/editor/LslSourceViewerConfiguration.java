/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Robert Greayer - modified extensively (renamed, etc.) to work with
 *                      LSL.
 *******************************************************************************/
package lslplus.editor;

import java.util.HashSet;
import java.util.Iterator;

import lslplus.LslPlusPlugin;
import lslplus.editor.imported.HTMLTextPresenter;
import lslplus.editor.lsl.LslCodeScanner;
import lslplus.editor.lsl.LslCompletionProcessor;
import lslplus.editor.lsl.LslPlusAutoIndentStrategy;
import lslplus.editor.lsl.LslPlusDoubleClickSelector;
import lslplus.editor.lsl.ScannerChangeListener;
import lslplus.util.LslColorProvider;

import org.eclipse.jface.text.DefaultIndentLineAutoEditStrategy;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.reconciler.IReconciler;
import org.eclipse.jface.text.reconciler.MonoReconciler;
import org.eclipse.jface.text.rules.BufferedRuleBasedScanner;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Shell;

/**
 * Configuration for an Lsl (+) source viewer.
 */
public class LslSourceViewerConfiguration extends SourceViewerConfiguration 
implements ScannerChangeListener {

    private HashSet<SourceViewerConfigurationListener> listeners = new HashSet<SourceViewerConfigurationListener>();
    private LslCodeScanner scanner;
    private LslPlusEditor editor;
    
    static class SingleTokenScanner extends BufferedRuleBasedScanner {
        public SingleTokenScanner(TextAttribute attribute) {
            setDefaultReturnToken(new Token(attribute));
        }
    }

    /**
     * Default constructor.
     * @param editor the editor
     */
    public LslSourceViewerConfiguration(LslPlusEditor editor) {
        this.scanner = LslPlusPlugin.getDefault().getLslCodeScanner();
        scanner.addListener(this);
        this.editor = editor;
    }
    
    public void addListener(SourceViewerConfigurationListener listener) {
        listeners.add(listener);
    }

    public void removeListener(SourceViewerConfigurationListener listener) {
        listeners.remove(listener);
    }
    
    /*
     * (non-Javadoc) Method declared on SourceViewerConfiguration
     */
    public IAnnotationHover getAnnotationHover(ISourceViewer sourceViewer) {
        return new LslAnnotationHover();
    }

    /*
     * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getAutoEditStrategies(org.eclipse.jface.text.source.ISourceViewer,
     *      java.lang.String)
     */
    public IAutoEditStrategy[] getAutoEditStrategies(ISourceViewer sourceViewer, String contentType) {
        IAutoEditStrategy strategy = (IDocument.DEFAULT_CONTENT_TYPE.equals(contentType) ? new LslPlusAutoIndentStrategy()
                : new DefaultIndentLineAutoEditStrategy());
        return new IAutoEditStrategy[] { strategy };
    }

    /*
     * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getConfiguredDocumentPartitioning(org.eclipse.jface.text.source.ISourceViewer)
     */
    public String getConfiguredDocumentPartitioning(ISourceViewer sourceViewer) {
        return LslPlusPlugin.LSL_PARTITIONING;
    }

    /*
     * (non-Javadoc) Method declared on SourceViewerConfiguration
     */
    public String[] getConfiguredContentTypes(ISourceViewer sourceViewer) {
        return new String[] { IDocument.DEFAULT_CONTENT_TYPE,
                LslPartitionScanner.LSL_MULTILINE_COMMENT };
    }

    /*
     * (non-Javadoc) Method declared on SourceViewerConfiguration
     */
    public IContentAssistant getContentAssistant(ISourceViewer sourceViewer) {

        ContentAssistant assistant = new ContentAssistant();
        assistant.setDocumentPartitioning(getConfiguredDocumentPartitioning(sourceViewer));
        assistant.setContentAssistProcessor(new LslCompletionProcessor(),
                IDocument.DEFAULT_CONTENT_TYPE);

        assistant.enableAutoActivation(true);
        assistant.setAutoActivationDelay(500);
        assistant.setProposalPopupOrientation(IContentAssistant.CONTEXT_INFO_BELOW);
        assistant.setProposalSelectorBackground(LslPlusPlugin.getDefault().getLslColorProvider()
                .getColor(new RGB(224, 224, 224)));
        assistant.setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_ABOVE);
        assistant.setContextInformationPopupBackground(LslPlusPlugin.getDefault()
                .getLslColorProvider().getColor(new RGB(255, 255, 50)));
        assistant.setInformationControlCreator(new IInformationControlCreator() {
            public IInformationControl createInformationControl(Shell parent) {
                return new DefaultInformationControl(parent, new HTMLTextPresenter(true));
            }
        });
        return assistant;
    }

    /*
     * (non-Javadoc) Method declared on SourceViewerConfiguration
     */
    public String getDefaultPrefix(ISourceViewer sourceViewer, String contentType) {
        return (IDocument.DEFAULT_CONTENT_TYPE.equals(contentType) ? "//" : null); //$NON-NLS-1$
    }

    /*
     * (non-Javadoc) Method declared on SourceViewerConfiguration
     */
    public ITextDoubleClickStrategy getDoubleClickStrategy(ISourceViewer sourceViewer,
            String contentType) {
        return new LslPlusDoubleClickSelector();
    }

    /*
     * (non-Javadoc) Method declared on SourceViewerConfiguration
     */
    public String[] getIndentPrefixes(ISourceViewer sourceViewer, String contentType) {
        return new String[] { "\t", "    " }; //$NON-NLS-1$ //$NON-NLS-2$
    }

    /*
     * (non-Javadoc) Method declared on SourceViewerConfiguration
     */
    public IPresentationReconciler getPresentationReconciler(ISourceViewer sourceViewer) {

        LslColorProvider provider = LslPlusPlugin.getDefault().getLslColorProvider();
        PresentationReconciler reconciler = new PresentationReconciler();
        reconciler.setDocumentPartitioning(getConfiguredDocumentPartitioning(sourceViewer));

        DefaultDamagerRepairer dr = new DefaultDamagerRepairer(LslPlusPlugin.getDefault()
                .getLslCodeScanner());
        reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
        reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);

        dr = new DefaultDamagerRepairer(new SingleTokenScanner(new TextAttribute(provider
                .getColor(LslColorProvider.MULTI_LINE_COMMENT_COLOR))));
        reconciler.setDamager(dr, LslPartitionScanner.LSL_MULTILINE_COMMENT);
        reconciler.setRepairer(dr, LslPartitionScanner.LSL_MULTILINE_COMMENT);

        return reconciler;
    }

    @Override
    public IReconciler getReconciler(ISourceViewer sourceViewer) {
    	LslPlusReconcilingStrategy strat = new LslPlusReconcilingStrategy(this.editor);
    	MonoReconciler reconciler = new MonoReconciler(strat, false);
    	return reconciler;
    }

    public int getTabWidth(ISourceViewer sourceViewer) {
        return 4;
    }

    public ITextHover getTextHover(ISourceViewer sourceViewer, String contentType) {
        return new LslTextHover();
    }

    public void dispose() {
        listeners.clear();
        scanner.removeListener(this);
    }

    public void scannerChanged() {
        Iterator<SourceViewerConfigurationListener> i = listeners.iterator();
        
        while (i.hasNext()) {
            i.next().configurationChanged();
        }
    }
    
    
}
