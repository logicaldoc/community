package com.logicaldoc.gui.frontend.client.system;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.logicaldoc.gui.frontend.client.system.task.TasksPanel;
import com.logicaldoc.gui.frontend.client.tenant.TenantsPanel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
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

		Button general = new Button(I18N.message("general"));
		general.setWidth100();
		general.setHeight(25);

		if (Menu.enabled(Menu.GENERAL))
			addMember(general);

		Button tasks = new Button(I18N.message("scheduledtasks"));
		tasks.setWidth100();
		tasks.setHeight(25);
		addMember(tasks);

		Button branding = new Button(I18N.message("branding"));
		branding.setWidth100();
		branding.setHeight(25);
		if (Feature.visible(Feature.BRANDING_STANDARD) && Menu.enabled(Menu.BRANDING)) {
			addMember(branding);
			if (!Feature.enabled(Feature.BRANDING_STANDARD)) {
				branding.setDisabled(true);
				branding.setTooltip(I18N.message("featuredisabled"));
			}
		}

		Button clustering = new Button(I18N.message("clustering"));
		clustering.setWidth100();
		clustering.setHeight(25);
		if (Feature.visible(Feature.CLUSTERING) && Menu.enabled(Menu.CLUSTERING) && Session.get().isDefaultTenant()) {
			addMember(clustering);
			if (!Feature.enabled(Feature.CLUSTERING)) {
				clustering.setDisabled(true);
				clustering.setTooltip(I18N.message("featuredisabled"));
			}
		}

		Button tenants = new Button(I18N.message("tenants"));
		tenants.setWidth100();
		tenants.setHeight(25);
		if (Feature.visible(Feature.MULTI_TENANT) && Menu.enabled(Menu.TENANTS) && Session.get().isDefaultTenant()) {
			addMember(tenants);
			if (!Feature.enabled(Feature.MULTI_TENANT)) {
				tenants.setDisabled(true);
				tenants.setTooltip(I18N.message("featuredisabled"));
			}
		}

		final Button updates = new Button(I18N.message("updates"));
		updates.setWidth100();
		updates.setHeight(25);
		updates.setVisible(false);
		addMember(updates);
		final Button confirmUpdate = new Button("<span style='color:red;'><b>" + I18N.message("confirmupdate")
				+ "</b></span>");
		confirmUpdate.setWidth100();
		confirmUpdate.setHeight(25);
		confirmUpdate.setVisible(false);
		addMember(confirmUpdate);

		if (Feature.visible(Feature.UPDATES) && Menu.enabled(Menu.UPDATES) && Session.get().isDefaultTenant()) {
			String runlevel = Session.get().getConfig("runlevel");
			if ("updated".equals(runlevel)) {
				confirmUpdate.setVisible(true);
				if (!Feature.enabled(Feature.UPDATES)) {
					confirmUpdate.setDisabled(true);
					confirmUpdate.setTooltip(I18N.message("featuredisabled"));
				}
			} else {
				updates.setVisible(true);
				if (!Feature.enabled(Feature.UPDATES)) {
					updates.setDisabled(true);
					updates.setTooltip(I18N.message("featuredisabled"));
				}
			}
		}

		Button productNews = new Button(I18N.message("task.name.ProductNews"));
		productNews.setWidth100();
		productNews.setHeight(25);
		if (Feature.visible(Feature.PRODUCT_NEWS)) {
			addMember(productNews);
			if (!Feature.enabled(Feature.PRODUCT_NEWS)) {
				productNews.setDisabled(true);
				productNews.setTooltip(I18N.message("featuredisabled"));
			}
		}

		if (Session.get().isDemo()) {
			clustering.setDisabled(true);
			tenants.setDisabled(true);
			updates.setDisabled(true);
			branding.setDisabled(true);
		}

		if (!Session.get().isDefaultTenant()) {
			clustering.setVisible(false);
			tenants.setVisible(false);
			updates.setVisible(false);
			confirmUpdate.setVisible(false);
		}

		addInformations();

		general.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new GeneralPanel());
			}
		});

		tasks.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new TasksPanel());
			}
		});

		productNews.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new ProductNewsPanel());
			}
		});

		updates.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new UpdatePanel());
			}
		});

		confirmUpdate.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SystemService.Instance.get().confirmUpdate(new AsyncCallback<Void>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(Void arg) {
						Session.get().getInfo().setConfig("runlevel", "default");
						confirmUpdate.setVisible(false);
						updates.setVisible(true);

						SC.say(I18N.message("confirmupdateresp") + "\n" + I18N.message("suggestedtorestart"));
					}
				});
			}
		});

		clustering.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SettingService.Instance.get().loadSettingsByNames(
						new String[] { "cluster.enabled", "cluster.name", "cluster.node.host", "cluster.node.port",
								"cluster.node.context", "cluster.port", "cluster.multicastip", "id" },
						new AsyncCallback<GUIParameter[]>() {

							@Override
							public void onFailure(Throwable caught) {
								Log.serverError(caught);
							}

							@Override
							public void onSuccess(GUIParameter[] settings) {
								AdminScreen.get().setContent(new ClusteringPanel(settings));
							}
						});
			}
		});

		tenants.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new TenantsPanel());
			}
		});

		branding.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new BrandingPanel(Session.get().getTenantId()));
			}
		});
	}

	private void addInformations() {
		DynamicForm form1 = new DynamicForm();
		form1.setWidth(300);
		form1.setColWidths(1, "*");

		StaticTextItem productName = ItemFactory.newStaticTextItem("productName", "", "<b>"
				+ Session.get().getInfo().getBranding().getProductName() + "</b>");
		productName.setShouldSaveValue(false);
		productName.setShowTitle(false);
		productName.setWrapTitle(false);
		productName.setWrap(false);
		productName.setEndRow(true);

		StaticTextItem version = ItemFactory.newStaticTextItem("version", "", I18N.message("version") + " "
				+ Session.get().getInfo().getRelease());
		version.setShouldSaveValue(false);
		version.setShowTitle(false);
		version.setWrap(false);
		version.setEndRow(true);

		StaticTextItem vendor = ItemFactory.newStaticTextItem("vendor", "", "&copy; "
				+ Session.get().getInfo().getBranding().getVendor());
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

		String mailTo = "mailto:" + Session.get().getInfo().getBranding().getSupport() + "?subject="
				+ Session.get().getInfo().getBranding().getProductName() + " Support - ";
		if (userno != null)
			mailTo += "UserNo(" + userno + ")";
		else
			mailTo += "ID(" + Session.get().getInfo().getInstallationId() + ")";
		support.setValue(mailTo);
		support.setRequired(true);
		support.setShouldSaveValue(false);

		StaticTextItem installationID = ItemFactory.newStaticTextItem("installid", "installid", installationId);
		installationID.setWidth(250);
		installationID.setRequired(true);
		installationID.setShouldSaveValue(false);
		installationID.setWrap(true);
		installationID.setWrapTitle(false);

		StaticTextItem usernoItem = ItemFactory.newStaticTextItem("userno", "userno", userno);
		usernoItem.setWidth(250);
		usernoItem.setRequired(true);
		usernoItem.setShouldSaveValue(false);
		usernoItem.setWrap(true);
		usernoItem.setWrapTitle(false);

		StaticTextItem hostName = ItemFactory.newStaticTextItem("hostname", "hostname", Session.get().getInfo()
				.getHostName());
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