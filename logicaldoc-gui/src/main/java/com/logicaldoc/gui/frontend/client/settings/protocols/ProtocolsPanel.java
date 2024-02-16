package com.logicaldoc.gui.frontend.client.settings.protocols;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel shows the Web Service, WebDAV and other protocols.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class ProtocolsPanel extends AdminPanel {

	private static final String FALSE = "false";

	private static final String ENABLED = "enabled";

	private ValuesManager vm = new ValuesManager();

	private GUIParameter cmisEnabled = null;

	private GUIParameter cmisChangelog = null;

	private GUIParameter cmisMaxItems = null;

	private GUIParameter wdEnabled = null;

	private GUIParameter wdDepth = null;

	private GUIParameter ftpEnabled = null;

	private GUIParameter ftpPort = null;

	private GUIParameter ftpSsl = null;

	private GUIParameter ftpKeystoreFile = null;

	private GUIParameter ftpKeystorePassword = null;

	private GUIParameter ftpKeystoreAlias = null;

	private GUIParameter ftpKeystoreAliasPassword = null;

	private WebservicesPanel webservicesPanel;

	public ProtocolsPanel() {
		super("webservice");
	}

	@Override
	protected void onDraw() {
		SettingService.Instance.get().loadProtocolSettings(new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(List<GUIParameter> settings) {
				init(settings);
			}
		});
	}

	private void init(List<GUIParameter> settings) {
		collectParameters(settings);

		webservicesPanel = new WebservicesPanel(settings, vm);
		body.setMembers(webservicesPanel);

		Tab cmis = new Tab();
		cmis.setTitle("CMIS");

		DynamicForm cmisForm = new DynamicForm();
		cmisForm.setValuesManager(vm);
		cmisForm.setTitleOrientation(TitleOrientation.LEFT);
		cmisForm.setNumCols(2);
		cmisForm.setColWidths(1, "*");
		cmisForm.setPadding(5);

		// Url
		StaticTextItem cmisUrl = ItemFactory.newStaticTextItem("cmisUrl", I18N.message("url"),
				GWT.getHostPageBaseURL() + "service/cmis");

		// CMIS Service Enabled
		RadioGroupItem cmisEnabledItem = ItemFactory.newBooleanSelector("cmisEnabled", ENABLED);
		cmisEnabledItem.setRequired(true);
		cmisEnabledItem.setWrapTitle(false);
		cmisEnabledItem.setValue(yesNo(cmisEnabled.getValue()));
		cmisEnabledItem.setDisabled(!Session.get().isDefaultTenant());

		// CMIS Changelog
		RadioGroupItem cmisChangelogItem = ItemFactory.newBooleanSelector("cmisChangelog", "changelog");
		cmisChangelogItem.setRequired(true);
		cmisChangelogItem.setWrapTitle(false);
		cmisChangelogItem.setValue(yesNo(cmisChangelog.getValue()));

		// CMIS Max Items
		SpinnerItem cmisMaxItemsItem = ItemFactory.newSpinnerItem("cmisMaxItems", "maxitems",
				Integer.parseInt(cmisMaxItems.getValue()));
		cmisMaxItemsItem.setRequired(true);
		cmisMaxItemsItem.setWrapTitle(false);
		cmisMaxItemsItem.setMin(0);
		cmisMaxItemsItem.setMax(Integer.MAX_VALUE);

		if (Session.get().isDefaultTenant())
			cmisForm.setItems(cmisUrl, cmisEnabledItem, cmisChangelogItem, cmisMaxItemsItem);
		else
			cmisForm.setItems(cmisUrl, cmisEnabledItem);
		cmis.setPane(cmisForm);

		Tab webDav = new Tab();
		webDav.setTitle(I18N.message("webdav"));
		DynamicForm webDavForm = new DynamicForm();
		webDavForm.setValuesManager(vm);
		webDavForm.setTitleOrientation(TitleOrientation.LEFT);
		webDavForm.setNumCols(2);
		webDavForm.setColWidths(1, "*");
		webDavForm.setPadding(5);

		// Urls
		StaticTextItem wdUrl = ItemFactory.newStaticTextItem("wdUrl", "WebDAV",
				GWT.getHostPageBaseURL() + "webdav/store");

		StaticTextItem wdbUrl = ItemFactory.newStaticTextItem("wdbUrl", "WebDAV basic",
				GWT.getHostPageBaseURL() + "webdavb");
		wdbUrl.setVisible(Feature.enabled(Feature.WEBDAV_BASIC));
		wdbUrl.setHint("&nbsp;&nbsp;&nbsp;(" + I18N.message("webdavbtooltip") + ")");

		// Status
		RadioGroupItem wdEnabledItem = ItemFactory.newBooleanSelector("wdEnabled", ENABLED);
		wdEnabledItem.setRequired(true);
		wdEnabledItem.setValue(yesNo(wdEnabled.getValue()));
		wdEnabledItem.setDisabled(!Session.get().isDefaultTenant());

		// Default depth
		SpinnerItem wdDepthItem = ItemFactory.newSpinnerItem("wdDepth", "depth",
				wdDepth.getValue() != null ? Long.parseLong(wdDepth.getValue()) : 1L);
		wdDepthItem.setRequired(true);
		wdDepthItem.setMin(0);
		wdDepthItem.setMax(Integer.MAX_VALUE);

		if (Session.get().isDefaultTenant())
			webDavForm.setItems(wdUrl, wdbUrl, wdEnabledItem, wdDepthItem);
		else
			webDavForm.setItems(wdUrl, wdbUrl, wdEnabledItem);
		webDav.setPane(webDavForm);

		// FTP Service status
		RadioGroupItem ftpEnabledItem = ItemFactory.newBooleanSelector("ftpEnabled", ENABLED);
		ftpEnabledItem.setRequired(true);
		ftpEnabledItem.setWrapTitle(false);
		ftpEnabledItem.setValue(yesNo(ftpEnabled.getValue()));
		ftpEnabledItem.setDisabled(!Session.get().isDefaultTenant());

		// FTP port
		IntegerItem ftpPortItem = ItemFactory.newIntegerItem("ftpPort", "port", 21);
		ftpPortItem.setRequired(true);
		ftpPortItem.setWrapTitle(false);
		ftpPortItem.setWidth(80);
		ftpPortItem.setValue(Integer.parseInt(ftpPort.getValue()));
		ftpPortItem.setDisabled(!Session.get().isDefaultTenant());

		// FTP security
		RadioGroupItem ftpSslItem = ItemFactory.newBooleanSelector("ftpSsl", "encryptionftps");
		ftpSslItem.setRequired(true);
		ftpSslItem.setWrapTitle(false);
		ftpSslItem.setValue(yesNo(ftpSsl.getValue()));
		ftpSslItem.setDisabled(!Session.get().isDefaultTenant());

		TextItem ftpKeystoreFileItem = ItemFactory.newTextItem("ftpKeystoreFile", "keystore",
				ftpKeystoreFile.getValue());
		ftpKeystoreFileItem.setRequired(false);
		ftpKeystoreFileItem.setWrapTitle(false);
		ftpKeystoreFileItem.setWidth(400);
		ftpKeystoreFileItem.setValue(ftpKeystoreFile.getValue());

		TextItem ftpKeystorePasswordItem = ItemFactory.newTextItem("ftpKeystorePassword", "keystorepasswd",
				ftpKeystorePassword.getValue());
		ftpKeystorePasswordItem.setRequired(false);
		ftpKeystorePasswordItem.setWrapTitle(false);
		ftpKeystorePasswordItem.setValue(ftpKeystorePassword.getValue());

		TextItem ftpKeystoreAliasItem = ItemFactory.newTextItem("ftpKeystoreAlias", "keystorealias",
				ftpKeystoreAlias.getValue());
		ftpKeystoreAliasItem.setRequired(false);
		ftpKeystoreAliasItem.setWrapTitle(false);
		ftpKeystoreAliasItem.setValue(ftpKeystoreAlias.getValue());

		TextItem ftpKeystoreAliasPasswordItem = ItemFactory.newTextItem("ftpKeystoreAliasPassword",
				"keystorealiaspassword", ftpKeystoreAliasPassword.getValue());
		ftpKeystoreAliasPasswordItem.setRequired(false);
		ftpKeystoreAliasPasswordItem.setWrapTitle(false);
		ftpKeystoreAliasPasswordItem.setValue(ftpKeystoreAliasPassword.getValue());

		Tab ftp = new Tab();
		ftp.setTitle(I18N.message("fftp"));
		DynamicForm ftpForm = new DynamicForm();
		ftpForm.setValuesManager(vm);
		ftpForm.setTitleOrientation(TitleOrientation.LEFT);
		ftpForm.setNumCols(2);
		ftpForm.setColWidths(1, "*");
		ftpForm.setPadding(5);

		if (Session.get().isDefaultTenant())
			ftpForm.setItems(ftpEnabledItem, ftpPortItem, ftpSslItem, ftpKeystoreFileItem, ftpKeystorePasswordItem,
					ftpKeystoreAliasItem, ftpKeystoreAliasPasswordItem);
		else {
			ftpForm.setItems(ftpEnabledItem, ftpPortItem, ftpSslItem);
		}

		ftp.setPane(ftpForm);

		addCMIS(cmis);

		addWebDAV(webDav);

		addFtp(ftp);

		addSaveButton();
	}

	private String yesNo(String trueFalse) {
		return trueFalse.equals("true") ? "yes" : "no";
	}

	private void addSaveButton() {
		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler((ClickEvent event) -> onSave());

		addMember(save);
		if (Session.get().isDemo()) {
			// In demo mode you cannot alter the client configurations
			save.setDisabled(true);
		}
	}

	private void onSave() {
		if (Boolean.FALSE.equals(vm.validate())) {
			SC.warn(I18N.message("invalidsettings"));
			return;
		}

		@SuppressWarnings("unchecked")
		Map<String, Object> values = vm.getValues();
		if (Session.get().isDefaultTenant()) {
			webservicesPanel.save();

			ProtocolsPanel.this.cmisEnabled.setValue(values.get("cmisEnabled").equals("yes") ? "true" : FALSE);

			ProtocolsPanel.this.cmisChangelog.setValue(values.get("cmisChangelog").equals("yes") ? "true" : FALSE);

			ProtocolsPanel.this.cmisMaxItems.setValue(values.get("cmisMaxItems").toString());

			ProtocolsPanel.this.wdEnabled.setValue(values.get("wdEnabled").equals("yes") ? "true" : FALSE);

			ProtocolsPanel.this.wdDepth.setValue(values.get("wdDepth").toString());

			ProtocolsPanel.this.ftpEnabled.setValue(values.get("ftpEnabled").equals("yes") ? "true" : FALSE);
			ProtocolsPanel.this.ftpPort.setValue(values.get("ftpPort").toString());
			ProtocolsPanel.this.ftpSsl.setValue(values.get("ftpSsl").equals("yes") ? "true" : FALSE);
			ProtocolsPanel.this.ftpKeystoreFile.setValue(values.get("ftpKeystoreFile").toString());
			ProtocolsPanel.this.ftpKeystoreAlias.setValue(values.get("ftpKeystoreAlias").toString());
			ProtocolsPanel.this.ftpKeystoreAliasPassword.setValue(values.get("ftpKeystoreAliasPassword").toString());
			ProtocolsPanel.this.ftpKeystorePassword.setValue(values.get("ftpKeystorePassword").toString());
		}

		List<GUIParameter> params = new ArrayList<>();
		params.addAll(webservicesPanel.getSettings());
		params.add(ProtocolsPanel.this.wdEnabled);
		params.add(ProtocolsPanel.this.wdDepth);
		params.add(ProtocolsPanel.this.cmisEnabled);
		params.add(ProtocolsPanel.this.cmisChangelog);
		params.add(ProtocolsPanel.this.cmisMaxItems);
		params.add(ProtocolsPanel.this.ftpEnabled);
		params.add(ProtocolsPanel.this.ftpPort);
		params.add(ProtocolsPanel.this.ftpSsl);
		params.add(ProtocolsPanel.this.ftpKeystoreFile);
		params.add(ProtocolsPanel.this.ftpKeystorePassword);
		params.add(ProtocolsPanel.this.ftpKeystoreAlias);
		params.add(ProtocolsPanel.this.ftpKeystoreAliasPassword);

		SettingService.Instance.get().saveSettings(params, new AsyncCallback<>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void ret) {
				GuiLog.info(I18N.message("settingssaved"), null);
				GuiLog.info(I18N.message("needrestart"), null);
			}
		});
	}

	private void addFtp(Tab ftp) {
		if (Feature.visible(Feature.FTP)) {
			tabs.addTab(ftp);
			if (!Feature.enabled(Feature.FTP))
				ftp.setPane(new FeatureDisabled());
		}
	}

	private void addWebDAV(Tab webDav) {
		if (Feature.visible(Feature.WEBDAV)) {
			tabs.addTab(webDav);
			if (!Feature.enabled(Feature.WEBDAV))
				webDav.setPane(new FeatureDisabled());
		}
	}

	private void addCMIS(Tab cmis) {
		if (Feature.visible(Feature.CMIS)) {
			tabs.addTab(cmis);
			if (!Feature.enabled(Feature.CMIS))
				cmis.setPane(new FeatureDisabled());
		}
	}

	private void collectParameters(List<GUIParameter> settings) {
		for (GUIParameter parameter : settings) {
			if (parameter.getName().equals("cmis.enabled"))
				cmisEnabled = parameter;
			else if (parameter.getName().equals("cmis.changelog"))
				cmisChangelog = parameter;
			else if (parameter.getName().equals("cmis.maxitems"))
				cmisMaxItems = parameter;
			else if (parameter.getName().equals("webdav.enabled"))
				wdEnabled = parameter;
			else if (parameter.getName().equals("webdav.depth"))
				wdDepth = parameter;
			else if (parameter.getName().equals("ftp.enabled"))
				ftpEnabled = parameter;
			else if (parameter.getName().equals("ftp.port"))
				ftpPort = parameter;
			else if (parameter.getName().equals("ftp.ssl"))
				ftpSsl = parameter;
			else if (parameter.getName().equals("ftp.keystore.file"))
				ftpKeystoreFile = parameter;
			else if (parameter.getName().equals("ftp.keystore.alias"))
				ftpKeystoreAlias = parameter;
			else if (parameter.getName().equals("ftp.keystore.alias.password"))
				ftpKeystoreAliasPassword = parameter;
			else if (parameter.getName().equals("ftp.keystore.password"))
				ftpKeystorePassword = parameter;
		}
	}
}