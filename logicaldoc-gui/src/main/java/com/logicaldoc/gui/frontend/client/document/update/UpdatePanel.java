package com.logicaldoc.gui.frontend.client.document.update;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.document.DocumentCapturePanel;
import com.logicaldoc.gui.frontend.client.document.DocumentExtendedPropertiesPanel;
import com.logicaldoc.gui.frontend.client.document.DocumentSecurityPanel;
import com.logicaldoc.gui.frontend.client.document.PublishingPanel;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel collects all documents details needed by a bulk update or other
 * update operations.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class UpdatePanel extends VLayout {
	protected GUIDocument document = new GUIDocument();

	protected Layout propertiesTabPanel;

	protected Layout extendedPropertiesTabPanel;

	protected Layout captureTabPanel;

	protected Layout retentionPoliciesTabPanel;

	protected Layout notificationTabPanel;

	protected Layout securityTabPanel;

	protected UpdateStandardPropertiesPanel propertiesPanel;

	protected DocumentExtendedPropertiesPanel extendedPropertiesPanel;

	protected PublishingPanel retentionPoliciesPanel;

	protected UpdateNotificationPanel notificationPanel;

	protected DocumentCapturePanel capturePanel;

	protected DocumentSecurityPanel securityPanel;

	protected TabSet tabSet = new TabSet();

	protected Tab propertiesTab;

	protected Tab extendedPropertiesTab;

	protected Tab retentionPoliciesTab;

	protected Tab notificationTab;

	protected Tab captureTab;

	protected Tab securityTab;

	public UpdatePanel(GUIDocument metadata, boolean showNotificationPanel, boolean showSecurityTab) {
		super();

		if (metadata != null)
			document = metadata;
		else {
			GUIFolder currentFolder = FolderController.get().getCurrentFolder();
			document = currentFolder.newDocument();
		}

		setHeight100();
		setWidth100();
		setMembersMargin(10);

		HTMLPane spacer = new HTMLPane();
		spacer.setContents("<div>&nbsp;</div>");
		spacer.setWidth("60%");
		spacer.setOverflow(Overflow.HIDDEN);

		prepareTabs();
		prepareTabset(showNotificationPanel, showSecurityTab);
		refresh();
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

		captureTab = new Tab(I18N.message("capture"));
		captureTabPanel = new HLayout();
		captureTabPanel.setWidth100();
		captureTabPanel.setHeight100();
		captureTab.setPane(captureTabPanel);

		retentionPoliciesTab = new Tab(I18N.message("publishing"));
		retentionPoliciesTabPanel = new HLayout();
		retentionPoliciesTabPanel.setWidth100();
		retentionPoliciesTabPanel.setHeight100();
		retentionPoliciesTab.setPane(retentionPoliciesTabPanel);

		notificationTab = new Tab(I18N.message("notifyusers"));
		notificationTabPanel = new HLayout();
		notificationTabPanel.setWidth100();
		notificationTabPanel.setHeight100();
		notificationTab.setPane(notificationTabPanel);

		securityTab = new Tab(I18N.message("security"));
		securityTabPanel = new HLayout();
		securityTabPanel.setWidth100();
		securityTabPanel.setHeight100();
		securityTab.setPane(securityTabPanel);
	}

	protected void prepareTabset(boolean showNotificationPanel, boolean showSecurityTab) {
		tabSet = new TabSet();
		tabSet.setTabBarPosition(Side.TOP);
		tabSet.setTabBarAlign(Side.LEFT);
		tabSet.setWidth100();
		tabSet.setHeight100();

		tabSet.addTab(propertiesTab);
		tabSet.addTab(extendedPropertiesTab);
		if (Feature.enabled(Feature.RETENTION_POLICIES) && (Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN)
				|| Session.get().getUser().isMemberOf(Constants.GROUP_PUBLISHER)))
			tabSet.addTab(retentionPoliciesTab);

		if (Menu.enabled(Menu.CAPTURE))
			tabSet.addTab(captureTab);

		if (showNotificationPanel)
			tabSet.addTab(notificationTab);

		if (showSecurityTab)
			tabSet.addTab(securityTab);

		addMember(tabSet);
	}

	protected void refresh() {
		/*
		 * Prepare the standard properties tab
		 */
		if (propertiesPanel != null) {
			propertiesPanel.destroy();
			if (Boolean.TRUE.equals(propertiesTabPanel.contains(propertiesPanel)))
				propertiesTabPanel.removeMember(propertiesPanel);
		}

		propertiesPanel = new UpdateStandardPropertiesPanel(document);
		propertiesTabPanel.addMember(propertiesPanel);


		ChangedHandler templateChangedHandler = event -> {
			document.setOcrTemplateId(null);
			document.setBarcodeTemplateId(null);
			capturePanel.refresh(document.getTemplateId());
		};

		ChangedHandler nothingToDo = event -> {
			// Nothing to do
		};
		
		/*
		 * Prepare the extended properties tab
		 */
		if (extendedPropertiesPanel != null) {
			extendedPropertiesPanel.destroy();
			if (Boolean.TRUE.equals(extendedPropertiesTabPanel.contains(extendedPropertiesPanel)))
				extendedPropertiesTabPanel.removeMember(extendedPropertiesPanel);
		}
		extendedPropertiesPanel = new DocumentExtendedPropertiesPanel(document, nothingToDo, templateChangedHandler);
		extendedPropertiesTabPanel.addMember(extendedPropertiesPanel);

		/*
		 * Prepare the OCR settings tab
		 */
		if (capturePanel != null) {
			capturePanel.destroy();
			if (Boolean.TRUE.equals(captureTabPanel.contains(capturePanel)))
				captureTabPanel.removeMember(capturePanel);
		}
		capturePanel = new DocumentCapturePanel(document, nothingToDo, false);
		captureTabPanel.addMember(capturePanel);

		/*
		 * Prepare the retention policies tab
		 */
		if (retentionPoliciesPanel != null) {
			retentionPoliciesPanel.destroy();
			if (Boolean.TRUE.equals(retentionPoliciesTabPanel.contains(retentionPoliciesPanel)))
				retentionPoliciesTabPanel.removeMember(retentionPoliciesPanel);
		}
		retentionPoliciesPanel = new PublishingPanel(document, nothingToDo);
		retentionPoliciesTabPanel.addMember(retentionPoliciesPanel);

		/*
		 * Prepare the notifications tab
		 */
		refreshNotificationsTab();

		/*
		 * Prepare the security tab
		 */
		refreshSecurityTab();
	}

	private void refreshNotificationsTab() {
		if (notificationPanel != null) {
			notificationPanel.destroy();
			if (Boolean.TRUE.equals(notificationTabPanel.contains(notificationPanel)))
				notificationTabPanel.removeMember(notificationPanel);
		}
		notificationPanel = new UpdateNotificationPanel(document);
		notificationTabPanel.addMember(notificationPanel);
	}

	private void refreshSecurityTab() {
		if (securityPanel != null) {
			securityPanel.destroy();
			if (Boolean.TRUE.equals(securityTabPanel.contains(securityPanel)))
				securityTabPanel.removeMember(securityPanel);
		}
		securityPanel = new DocumentSecurityPanel(document);
		securityTabPanel.addMember(securityPanel);
	}

	public GUIDocument getDocument() {
		return document;
	}

	public boolean validate() {
		boolean stdValid = propertiesPanel.validate();
		boolean extValid = extendedPropertiesPanel.validate();
		boolean publishingValid = retentionPoliciesPanel.validate();
		boolean captureValid = capturePanel.validate();
		notificationPanel.validate();
		securityPanel.validate();

		if (!stdValid)
			tabSet.selectTab(propertiesTab);
		else if (!extValid)
			tabSet.selectTab(extendedPropertiesTab);
		else if (!publishingValid)
			tabSet.selectTab(retentionPoliciesTab);
		else if (!captureValid)
			tabSet.selectTab(captureTab);
		return stdValid && extValid && publishingValid && captureValid;
	}
}