package com.logicaldoc.gui.frontend.client.system;

import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.logicaldoc.gui.common.client.widgets.ApplicationRestarting;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.logicaldoc.gui.frontend.client.system.task.TasksPanel;
import com.logicaldoc.gui.frontend.client.system.update.UpdateAndPatchPanel;
import com.logicaldoc.gui.frontend.client.tenant.TenantsPanel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.LinkItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the administration system menu
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class SystemMenu extends VLayout {

	public SystemMenu() {
		setMargin(10);
		setMembersMargin(5);
		setOverflow(Overflow.AUTO);

		addGeneralButton();

		addTasksButton();

		addBrandingButton();

		addClusteringButton();

		addTenantsButton();

		Button updates = addUpdatesButton();

		addConfirmUpdate(updates);

		addLicenseButton();

		addRestartButton();

		addInformations();
	}

	private void addRestartButton() {
		final Button restart = new Button(I18N.message("restart"));
		restart.setWidth100();
		restart.setHeight(25);
		restart.addClickHandler(event -> SC.ask(I18N.message("restartalert"), answer -> {
			if (Boolean.TRUE.equals(answer)) {
				SystemService.Instance.get().restart(new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg) {
						ApplicationRestarting.get().show();

						restart.setDisabled(true);
						final String tenant = Session.get().getUser().getTenant().getName();
						Session.get().close();
						CookiesManager.removeSid();

						Timer timer = new Timer() {
							public void run() {
								Util.waitForUpAndRunning(tenant, I18N.getLocale());
							}
						};
						timer.schedule(6000);
					}
				});
			}
		}));
		if (Menu.enabled(Menu.RESTART) && Session.get().isDefaultTenant())
			addMember(restart);

		restart.setDisabled(Session.get().isDemo());
		restart.setVisible(Session.get().isDefaultTenant());
	}

	private void addLicenseButton() {
		final Button license = new Button(I18N.message("license"));
		license.setWidth100();
		license.setHeight(25);
		license.addClickHandler(event -> WindowUtils.openUrlInNewTab(Util.licenseUrl()));
		if (Session.get().isDefaultTenant() && "admin".equals(Session.get().getUser().getUsername())
				&& !Session.get().isDemo() && Feature.enabled(Feature.LICENSE)) {
			addMember(license);
		}
		license.setDisabled(Session.get().isDemo());
		license.setVisible(Session.get().isDefaultTenant());
	}

	private void addConfirmUpdate(Button updatesButton) {
		Button confirmUpdate = new Button(
				"<span style='color:red;'><b>" + I18N.message("confirmupdate") + "</b></span>");
		confirmUpdate.setWidth100();
		confirmUpdate.setHeight(25);
		confirmUpdate.addClickHandler(event -> SystemService.Instance.get().confirmUpdate(new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg) {
				Session.get().getInfo().setConfig("runlevel", "default");
				confirmUpdate.setVisible(false);
				updatesButton.setVisible(true);

				SC.say(I18N.message("confirmupdateresp") + "\n" + I18N.message("suggestedtorestart"));
			}
		}));

		if ((Feature.enabled(Feature.UPDATES) || Feature.enabled(Feature.PATCHES))
				&& Menu.enabled(Menu.UPDATES_AND_PATCHES) && Session.get().isDefaultTenant()) {
			addMember(confirmUpdate);

			String runlevel = Session.get().getConfig("runlevel");
			if ("updated".equals(runlevel)) {
				confirmUpdate.setVisible(true);
				updatesButton.setVisible(false);
			} else {
				updatesButton.setVisible(true);
				confirmUpdate.setVisible(false);
			}
		}
	}

	private Button addUpdatesButton() {
		Button updates = new Button(I18N.message("updatesandpatches"));
		updates.setWidth100();
		updates.setHeight(25);
		updates.addClickHandler(event -> AdminScreen.get().setContent(new UpdateAndPatchPanel()));
		addMember(updates);
		updates.setDisabled(Session.get().isDemo());
		updates.setVisible(Session.get().isDefaultTenant());
		return updates;
	}

	private void addTenantsButton() {
		Button tenants = new Button(I18N.message("tenants"));
		tenants.setWidth100();
		tenants.setHeight(25);
		tenants.addClickHandler(event -> AdminScreen.get().setContent(new TenantsPanel()));
		if (Feature.visible(Feature.MULTI_TENANT) && Menu.enabled(Menu.TENANTS) && Session.get().isDefaultTenant()) {
			addMember(tenants);
			if (!Feature.enabled(Feature.MULTI_TENANT))
				setFeatureDisabled(tenants);
		}
		tenants.setDisabled(Session.get().isDemo());
		tenants.setVisible(Session.get().isDefaultTenant());
	}

	private void addClusteringButton() {
		Button clustering = new Button(I18N.message("clustering"));
		clustering.setWidth100();
		clustering.setHeight(25);
		clustering.addClickHandler(event -> AdminScreen.get().setContent(new ClusteringPanel()));
		if (Feature.visible(Feature.CLUSTERING) && Menu.enabled(Menu.CLUSTERING) && Session.get().isDefaultTenant()) {
			addMember(clustering);
			if (!Feature.enabled(Feature.CLUSTERING))
				setFeatureDisabled(clustering);
		}
		clustering.setDisabled(Session.get().isDemo());
		clustering.setVisible(Session.get().isDefaultTenant());
	}

	private void setFeatureDisabled(Button button) {
		button.setDisabled(true);
		button.setTooltip(I18N.message("featuredisabled"));
	}

	private void addBrandingButton() {
		Button branding = new Button(I18N.message("branding"));
		branding.setWidth100();
		branding.setHeight(25);
		branding.addClickHandler(event -> AdminScreen.get().setContent(new BrandingPanel(Session.get().getTenantId())));
		if (Feature.visible(Feature.BRANDING_STANDARD) && Menu.enabled(Menu.BRANDING)) {
			addMember(branding);
			if (!Feature.enabled(Feature.BRANDING_STANDARD))
				setFeatureDisabled(branding);
		}
		branding.setDisabled(Session.get().isDemo());
	}

	private void addTasksButton() {
		Button tasks = new Button(I18N.message("scheduledtasks"));
		tasks.setWidth100();
		tasks.setHeight(25);
		tasks.addClickHandler(event -> AdminScreen.get().setContent(new TasksPanel()));
		addMember(tasks);
	}

	private void addGeneralButton() {
		Button general = new Button(I18N.message("general"));
		general.setWidth100();
		general.setHeight(25);
		general.addClickHandler((ClickEvent event) -> AdminScreen.get().setContent(new GeneralPanel()));
		if (Menu.enabled(Menu.GENERAL))
			addMember(general);
	}

	private void addInformations() {
		DynamicForm form1 = new DynamicForm();
		form1.setWidth(300);
		form1.setColWidths(1, "*");

		StaticTextItem productName = ItemFactory.newStaticTextItem("productName", "",
				"<b>" + Session.get().getInfo().getBranding().getProductName() + "</b>");
		productName.setShouldSaveValue(false);
		productName.setShowTitle(false);
		productName.setWrapTitle(false);
		productName.setWrap(false);
		productName.setEndRow(true);

		StaticTextItem version = ItemFactory.newStaticTextItem("version", "",
				I18N.message("version") + " " + Session.get().getInfo().getRelease());
		version.setShouldSaveValue(false);
		version.setShowTitle(false);
		version.setWrap(false);
		version.setEndRow(true);

		StaticTextItem vendor = ItemFactory.newStaticTextItem("vendor", "",
				"&copy; " + Session.get().getInfo().getBranding().getVendor());
		vendor.setShouldSaveValue(false);
		vendor.setShowTitle(false);
		vendor.setEndRow(true);

		String userno = Session.get().getInfo().getUserNo();
		String installationId = Session.get().getInfo().getInstallationId();

		DynamicForm form2 = new DynamicForm();
		form2.setAlign(Alignment.LEFT);
		form2.setTitleOrientation(TitleOrientation.TOP);
		form2.setColWidths(1);
		form2.setWrapItemTitles(false);
		form2.setNumCols(1);

		LinkItem support = new LinkItem();
		support.setName(I18N.message("support"));
		support.setLinkTitle(Session.get().getInfo().getBranding().getSupport());
		support.setVisible(Feature.enabled(Feature.TECHNICAL_SUPPORT));

		String mailTo = "mailto:" + Session.get().getInfo().getBranding().getSupport() + "?subject="
				+ Session.get().getInfo().getBranding().getProductName() + " Support - ";
		if (userno != null)
			mailTo += "UserNo(" + userno + ")";
		else
			mailTo += "ID(" + Session.get().getInfo().getInstallationId() + ")";
		support.setValue(mailTo);
		support.setRequired(true);
		support.setShouldSaveValue(false);

		StaticTextItem installationID = ItemFactory.newStaticTextItem("installid", installationId);
		installationID.setWidth(250);
		installationID.setRequired(true);
		installationID.setShouldSaveValue(false);
		installationID.setWrap(false);
		installationID.setWrapTitle(false);

		StaticTextItem usernoItem = ItemFactory.newStaticTextItem("userno", I18N.message("clicktoshow").toLowerCase());
		usernoItem.setWidth(250);
		usernoItem.setRequired(true);
		usernoItem.setShouldSaveValue(false);
		usernoItem.setWrap(false);
		usernoItem.setWrapTitle(false);
		usernoItem.addClickHandler(click -> usernoItem.setValue(userno));

		StaticTextItem hostName = ItemFactory.newStaticTextItem("hostname", Session.get().getInfo().getHostName());
		hostName.setWidth(250);
		hostName.setRequired(true);
		hostName.setShouldSaveValue(false);
		hostName.setWrap(true);
		hostName.setWrapTitle(false);
		hostName.setVisible(!Session.get().isDemo());

		form1.setItems(productName, version, vendor);

		if (userno != null)
			form2.setItems(support, usernoItem, installationID, hostName);
		else
			form2.setItems(support, installationID, hostName);

		if (!Session.get().isDemo()) {
			addMember(form1);
			addMember(form2);
		}
	}
}