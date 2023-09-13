package com.logicaldoc.gui.frontend.client.document;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.ServerValidationException;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.controllers.DocumentObserver;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.document.note.NotesPanel;
import com.logicaldoc.gui.frontend.client.document.signature.DigitalSignaturePanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects all documents details
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class DocumentDetailsPanel extends VLayout implements DocumentObserver {
	private static final String ID_TAB_ALIASES = "tab-aliases";

	private static final String ID_TAB_SUBSCRIPTIONS = "tab-subscriptions";

	protected GUIDocument document;

	protected String originalExtension;

	protected Layout propertiesTabPanel;

	protected Layout extendedPropertiesTabPanel;

	protected Layout versionsTabPanel;

	protected Layout historyTabPanel;

	protected Layout signatureTabPanel;

	protected Layout aliasesTabPanel;

	protected Layout linksTabPanel;

	protected Layout notesTabPanel;

	protected Layout previewTabPanel;

	protected Layout retentionPoliciesTabPanel;

	protected Layout calendarTabPanel;

	protected Layout subscriptionsTabPanel;

	protected Layout captureTabPanel;

	protected StandardPropertiesPanel propertiesPanel;

	protected DocumentExtendedPropertiesPanel extendedPropertiesPanel;

	protected VersionsPanel versionsPanel;

	protected DocumentHistoryPanel historyPanel;

	protected DigitalSignaturePanel signaturePanel;

	protected AliasesPanel aliasesPanel;

	protected LinksPanel linksPanel;

	protected NotesPanel notesPanel;

	protected DetailsPreviewPanel previewPanel;

	protected DocumentCalendarPanel calendarPanel;

	protected DocumentSubscriptionsPanel subscriptionsPanel;

	protected PublishingPanel retentionPoliciesPanel;

	protected DocumentCapturePanel capturePanel;

	protected EditingTabSet tabSet;

	protected Tab propertiesTab;

	protected Tab extendedPropertiesTab;

	protected Tab linksTab;

	protected Tab notesTab;

	protected Tab versionsTab;

	protected Tab historyTab;

	protected Tab signatureTab;

	protected Tab aliasesTab;

	protected Tab previewTab;

	protected Tab retentionPoliciesTab;

	protected Tab calendarTab;

	protected Tab subscriptionsTab;

	protected Tab captureTab;

	public DocumentDetailsPanel() {
		super();

		DocumentController.get().addObserver(this);

		setHeight100();
		setWidth100();
		setMembersMargin(10);

		prepareTabs();
		prepareTabset();
	}

	public void selectDefaultTab() {
		if ("preview".equals(Session.get().getInfo().getConfig("gui.document.tab")))
			tabSet.selectTab(previewTab);
		else
			tabSet.selectTab(propertiesTab);
	}

	protected void prepareTabs() {
		propertiesTab = new Tab(I18N.message("properties"));
		propertiesTabPanel = new HLayout();
		propertiesTabPanel.setWidth100();
		propertiesTabPanel.setHeight100();
		propertiesTab.setPane(propertiesTabPanel);

		extendedPropertiesTab = new Tab(I18N.message("propertiesext"));
		extendedPropertiesTabPanel = new HLayout();
		extendedPropertiesTabPanel.setWidth100();
		extendedPropertiesTabPanel.setHeight100();
		extendedPropertiesTab.setPane(extendedPropertiesTabPanel);

		linksTab = new Tab(I18N.message("links"));
		linksTabPanel = new HLayout();
		linksTabPanel.setWidth100();
		linksTabPanel.setHeight100();
		linksTab.setPane(linksTabPanel);

		notesTab = new Tab(I18N.message("notes"));
		notesTabPanel = new HLayout();
		notesTabPanel.setWidth100();
		notesTabPanel.setHeight100();
		notesTab.setPane(notesTabPanel);

		versionsTab = new Tab(I18N.message("versions"));
		versionsTabPanel = new HLayout();
		versionsTabPanel.setWidth100();
		versionsTabPanel.setHeight100();
		versionsTab.setPane(versionsTabPanel);

		historyTab = new Tab(I18N.message("history"));
		historyTabPanel = new HLayout();
		historyTabPanel.setWidth100();
		historyTabPanel.setHeight100();
		historyTab.setPane(historyTabPanel);

		signatureTab = new Tab(I18N.message("signature"));
		signatureTabPanel = new HLayout();
		signatureTabPanel.setWidth100();
		signatureTabPanel.setHeight100();
		signatureTab.setPane(signatureTabPanel);

		aliasesTab = new Tab(I18N.message("aliases"));
		aliasesTab.setID(ID_TAB_ALIASES);
		aliasesTabPanel = new HLayout();
		aliasesTabPanel.setWidth100();
		aliasesTabPanel.setHeight100();
		aliasesTab.setPane(aliasesTabPanel);

		previewTab = new Tab(I18N.message("preview"));
		previewTabPanel = new HLayout();
		previewTabPanel.setWidth100();
		previewTabPanel.setHeight100();
		previewTab.setPane(previewTabPanel);

		retentionPoliciesTab = new Tab(I18N.message("publishing"));
		retentionPoliciesTabPanel = new HLayout();
		retentionPoliciesTabPanel.setWidth100();
		retentionPoliciesTabPanel.setHeight100();
		retentionPoliciesTab.setPane(retentionPoliciesTabPanel);

		captureTab = new Tab(I18N.message("capture"));
		captureTabPanel = new HLayout();
		captureTabPanel.setWidth100();
		captureTabPanel.setHeight100();
		captureTab.setPane(captureTabPanel);

		calendarTab = new Tab(I18N.message("calendar"));
		calendarTabPanel = new HLayout();
		calendarTabPanel.setWidth100();
		calendarTabPanel.setHeight100();
		calendarTab.setPane(calendarTabPanel);

		subscriptionsTab = new Tab(I18N.message("subscriptions"));
		subscriptionsTab.setID(ID_TAB_SUBSCRIPTIONS);
		subscriptionsTabPanel = new HLayout();
		subscriptionsTabPanel.setWidth100();
		subscriptionsTabPanel.setHeight100();
		subscriptionsTab.setPane(subscriptionsTabPanel);
	}

	protected void prepareTabset() {
		tabSet = new EditingTabSet(event -> onSave(), event ->
		// We have to reload the document because the tags may be
		// reverted to the original tags list.
		// This 'if condition' is necessary to know if the close image
		// has been selected into the Documents list panel or into the
		// Search list panel.
		DocumentService.Instance.get().getById(document.getId(), new AsyncCallback<GUIDocument>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIDocument doc) {
				DocumentController.get().selected(doc);
			}
		}));

		tabSet.addTab(propertiesTab);

		tabSet.addTab(extendedPropertiesTab);

		if (Menu.enabled(Menu.VERSIONS))
			tabSet.addTab(versionsTab);

		if (Menu.enabled(Menu.PREVIEW))
			tabSet.addTab(previewTab);

		if (Feature.visible(Feature.NOTES)) {
			tabSet.addTab(notesTab);
			notesTab.setDisabled(!Feature.enabled(Feature.NOTES));
		}

		tabSet.addTab(linksTab);

		if (Menu.enabled(Menu.HISTORY)) {
			tabSet.addTab(historyTab);
		}

		if (Feature.visible(Feature.DIGITAL_SIGNATURE) && Menu.enabled(Menu.SIGNATURE)) {
			tabSet.addTab(signatureTab);
			signatureTab.setDisabled(!Feature.enabled(Feature.DIGITAL_SIGNATURE));
		}

		if ((Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN)
				|| Session.get().getUser().isMemberOf(Constants.GROUP_PUBLISHER))
				&& Feature.visible(Feature.RETENTION_POLICIES))
			tabSet.addTab(retentionPoliciesTab);
		retentionPoliciesTab.setDisabled(!Feature.enabled(Feature.RETENTION_POLICIES));

		if (Feature.visible(Feature.CALENDAR) && Menu.enabled(Menu.DOCUMENT_CALENDAR)) {
			tabSet.addTab(calendarTab);
			calendarTab.setDisabled(!Feature.enabled(Feature.CALENDAR));
		}

		if (Feature.visible(Feature.AUDIT)) {
			tabSet.addTab(subscriptionsTab);
			subscriptionsTab.setDisabled(!Feature.enabled(Feature.AUDIT));
		}

		if (Menu.enabled(Menu.ALIASES))
			tabSet.addTab(aliasesTab);

		if (Menu.enabled(Menu.CAPTURE))
			tabSet.addTab(captureTab);

		addMember(tabSet);
	}

	protected void refresh() {
		hideSave();

		ChangedHandler changeHandler = event -> displaySave();

		/*
		 * Prepare the standard properties tab
		 */
		prepareStandardPropertiesTab(changeHandler);

		/*
		 * Prepare the extended properties tab
		 */
		prepareExtendedPropertiesTab(changeHandler);

		/*
		 * Prepare the retention policies tab
		 */
		prepareRetentionPoliciesTab(changeHandler);

		/*
		 * Prepare the OCR tab
		 */
		prepareCaptureTab(changeHandler);

		/*
		 * Prepare the versions tab
		 */
		prepareVersionsTab();

		/*
		 * Prepare the history tab
		 */
		prepareHistoryTab();

		/*
		 * Prepare the aliases tab
		 */
		prepareAliasesTab();

		/*
		 * Prepare the links tab
		 */
		prepareLInksTab();

		/*
		 * Prepare the signature tab
		 */
		prepareSignatureTab();

		/*
		 * Prepare the notes tab
		 */
		prepareNotesTab();

		/*
		 * Prepare the preview tab
		 */
		preparePreviewTab();

		/*
		 * Prepare the calendar tab
		 */
		prepareCalendarTab();

		/*
		 * Prepare the subscriptions tab
		 */
		prepareSubscriptionsTab();
	}

	private void prepareSubscriptionsTab() {
		if (Feature.visible(Feature.AUDIT)) {
			if (subscriptionsPanel != null) {
				subscriptionsPanel.destroy();
				if (Boolean.TRUE.equals(subscriptionsTabPanel.contains(subscriptionsPanel)))
					subscriptionsTabPanel.removeMember(subscriptionsPanel);
			}

			if (document.getFolder().hasPermission(Constants.PERMISSION_SUBSCRIPTION)) {
				try {
					subscriptionsPanel = new DocumentSubscriptionsPanel(document);
					subscriptionsTabPanel.addMember(subscriptionsPanel);
					if (tabSet.getTab(ID_TAB_SUBSCRIPTIONS) == null)
						tabSet.addTab(subscriptionsTab);
				} catch (Exception t) {
					// Nothing to do
				}
			} else
				tabSet.removeTab(subscriptionsTab);
		}
	}

	private void prepareCalendarTab() {
		if (calendarPanel != null) {
			calendarPanel.destroy();
			if (Boolean.TRUE.equals(calendarTabPanel.contains(calendarPanel)))
				calendarTabPanel.removeMember(calendarPanel);
		}
		try {
			calendarPanel = new DocumentCalendarPanel(document);
			calendarTabPanel.addMember(calendarPanel);
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private void preparePreviewTab() {
		if (previewPanel != null) {
			previewPanel.destroy();
			if (Boolean.TRUE.equals(previewTabPanel.contains(previewPanel)))
				previewTabPanel.removeMember(previewPanel);
		}
		try {
			previewPanel = new DetailsPreviewPanel(document);
			previewTabPanel.addMember(previewPanel);
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private void prepareNotesTab() {
		if (notesPanel != null) {
			notesPanel.destroy();
			if (Boolean.TRUE.equals(notesTabPanel.contains(notesPanel)))
				notesTabPanel.removeMember(notesPanel);
		}
		try {
			notesPanel = new NotesPanel(document);
			notesTabPanel.addMember(notesPanel);
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private void prepareSignatureTab() {
		if (signaturePanel != null) {
			signaturePanel.destroy();
			if (Boolean.TRUE.equals(signatureTabPanel.contains(signaturePanel)))
				signatureTabPanel.removeMember(signaturePanel);
		}
		try {
			signaturePanel = new DigitalSignaturePanel(document);
			signatureTabPanel.addMember(signaturePanel);
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private void prepareLInksTab() {
		if (linksPanel != null) {
			linksPanel.destroy();
			if (Boolean.TRUE.equals(linksTabPanel.contains(linksPanel)))
				linksTabPanel.removeMember(linksPanel);
		}

		try {
			linksPanel = new LinksPanel(document);
			linksTabPanel.addMember(linksPanel);
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private void prepareAliasesTab() {
		if (aliasesPanel != null) {
			aliasesPanel.destroy();
			if (Boolean.TRUE.equals(aliasesTabPanel.contains(aliasesPanel)))
				aliasesTabPanel.removeMember(aliasesPanel);
		}

		if (document.getDocRef() == null && Menu.enabled(Menu.ALIASES)) {
			try {
				aliasesPanel = new AliasesPanel(document);
				aliasesTabPanel.addMember(aliasesPanel);
				if (tabSet.getTab(ID_TAB_ALIASES) == null)
					tabSet.addTab(aliasesTab);
			} catch (Exception t) {
				// Nothing to do
			}
		} else
			tabSet.removeTab(aliasesTab);
	}

	private void prepareHistoryTab() {
		if (historyPanel != null) {
			historyPanel.destroy();
			if (Boolean.TRUE.equals(historyTabPanel.contains(historyPanel)))
				historyTabPanel.removeMember(historyPanel);
		}
		try {
			historyPanel = new DocumentHistoryPanel(document);
			historyTabPanel.addMember(historyPanel);
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private void prepareVersionsTab() {
		if (versionsPanel != null) {
			versionsPanel.destroy();
			if (Boolean.TRUE.equals(versionsTabPanel.contains(versionsPanel)))
				versionsTabPanel.removeMember(versionsPanel);
		}
		try {
			versionsPanel = new VersionsPanel(document);
			versionsTabPanel.addMember(versionsPanel);
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private void prepareCaptureTab(ChangedHandler changeHandler) {
		if (capturePanel != null) {
			capturePanel.destroy();
			if (Boolean.TRUE.equals(captureTabPanel.contains(capturePanel)))
				captureTabPanel.removeMember(capturePanel);
		}
		try {
			capturePanel = new DocumentCapturePanel(document, changeHandler, true);
			captureTabPanel.addMember(capturePanel);
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private void prepareStandardPropertiesTab(ChangedHandler changeHandler) {
		if (propertiesPanel != null) {
			propertiesPanel.destroy();
			if (Boolean.TRUE.equals(propertiesTabPanel.contains(propertiesPanel)))
				propertiesTabPanel.removeMember(propertiesPanel);
		}

		try {
			propertiesPanel = new StandardPropertiesPanel(document, changeHandler);
			propertiesTabPanel.addMember(propertiesPanel);
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private void prepareRetentionPoliciesTab(ChangedHandler changeHandler) {
		if (retentionPoliciesPanel != null) {
			retentionPoliciesPanel.destroy();
			if (Boolean.TRUE.equals(retentionPoliciesTabPanel.contains(retentionPoliciesPanel)))
				retentionPoliciesTabPanel.removeMember(retentionPoliciesPanel);
		}
		try {
			retentionPoliciesPanel = new PublishingPanel(document, changeHandler);
			retentionPoliciesTabPanel.addMember(retentionPoliciesPanel);
		} catch (Exception t) {
			// Nothing to do
		}
	}

	private void prepareExtendedPropertiesTab(ChangedHandler changeHandler) {
		if (extendedPropertiesPanel != null) {
			extendedPropertiesPanel.destroy();
			if (Boolean.TRUE.equals(extendedPropertiesTabPanel.contains(extendedPropertiesPanel)))
				extendedPropertiesTabPanel.removeMember(extendedPropertiesPanel);
		}
		ChangedHandler templateChangedHandler = (ChangedEvent event) -> {
			document.setOcrTemplateId(null);
			document.setBarcodeTemplateId(null);
			if (capturePanel != null)
				capturePanel.refresh(document.getTemplateId());
		};
		try {
			extendedPropertiesPanel = new DocumentExtendedPropertiesPanel(document, changeHandler,
					templateChangedHandler);
			extendedPropertiesTabPanel.addMember(extendedPropertiesPanel);
		} catch (Exception t) {
			// Nothing to do
		}
	}

	public GUIDocument getDocument() {
		return document;
	}

	public void setDocument(GUIDocument document) {
		this.document = document;
		this.originalExtension = Util.getExtension(document.getFileName());
		refresh();
	}

	private void hideSave() {
		DocumentController.get().cancelEditing(document);
		tabSet.hideSave();
	}

	private void displaySave() {
		DocumentController.get().beginEditing(document);
		tabSet.displaySave();
	}

	private boolean validate() {
		boolean stdValid = propertiesPanel.validate();
		boolean extValid = extendedPropertiesPanel.validate();
		boolean publishingValid = retentionPoliciesPanel.validate();
		boolean ocrValid = capturePanel.validate();
		if (!stdValid)
			tabSet.selectTab(0);
		else if (!extValid)
			tabSet.selectTab(1);
		else if (!publishingValid)
			tabSet.selectTab(2);
		else if (!ocrValid)
			tabSet.selectTab(captureTab);
		return stdValid && extValid && publishingValid && ocrValid;
	}

	public void onSave() {
		if (validate()) {
			try {
				// Check if the user has changed the extension and warn him
				if (!originalExtension.equalsIgnoreCase(Util.getExtension(document.getFileName()))) {
					LD.ask(I18N.message("filename"), I18N.message("extchangewarn"), (Boolean value) -> {
						if (Boolean.TRUE.equals(value))
							saveDocument();
					});
				} else {
					saveDocument();
				}
			} catch (Exception t) {
				SC.warn(t.getMessage());
			}
		}
	}

	private void saveDocument() {
		if (Session.get().getConfigAsBoolean("gui.onsave.askversioncomment")) {
			LD.askForString(I18N.message("versioncomment"), I18N.message("versioncomment"), null, comment -> {
				document.setComment(comment);
				save();
			});
		} else {
			save();
		}
	}

	private void save() {
		DocumentService.Instance.get().save(document, new AsyncCallback<GUIDocument>() {
			@Override
			public void onFailure(Throwable caught) {
				if (caught instanceof ServerValidationException) {
					handleValidationException((ServerValidationException) caught);
				} else {
					GuiLog.serverError(caught);
				}
			}

			@Override
			public void onSuccess(GUIDocument result) {
				hideSave();
				setDocument(result);
				DocumentController.get().modified(result);

				// If the document is an alias we should alter the file name
				if (document.getDocRef() != null) {
					result.setId(document.getDocRef());
					result.setDocRef(document.getId());
					result.setFileName(document.getFileName());
					result.setStatus(GUIDocument.DOC_UNLOCKED);
					DocumentController.get().modified(result);
				}
			}
		});
	}

	private void handleValidationException(ServerValidationException e) {
		extendedPropertiesPanel.handleErrors(e);
	}

	@Override
	public void onDocumentSelected(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentModified(GUIDocument document) {
		if (this.document != null && this.document.getId() == document.getId()
				&& (!DocumentController.get().isEditing(document) || (document.getLockUserId() != null
						&& document.getLockUserId().longValue() != Session.get().getUser().getId())))
			setDocument(document);
	}

	@Override
	public void onDocumentStored(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentCheckedIn(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentCheckedOut(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentLocked(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentUnlocked(GUIDocument document) {
		onDocumentModified(document);
	}

	@Override
	public void onDocumentsDeleted(GUIDocument[] documents) {
		if (document != null && documents != null)
			for (GUIDocument deletedDoc : documents) {
				if (deletedDoc.getId() == document.getId()) {
					removeMembers(getMembers());
					return;
				}
			}
	}

	@Override
	public void onDocumentMoved(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentBeginEditing(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void onDocumentCancelEditing(GUIDocument document) {
		// Nothing to do
	}

	@Override
	public void destroy() {
		DocumentController.get().removeObserver(this);
	}

	@Override
	protected void onUnload() {
		destroy();
		super.onUnload();
	}

	@Override
	protected void onDestroy() {
		destroy();
		super.onDestroy();
	}
}