package com.logicaldoc.gui.frontend.client.settings;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIExternalCall;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.common.client.widgets.ToastNotification;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel shows the Web Service and WebDAV settings, and also the external
 * applications.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class ExternalAppsPanel extends AdminPanel {

	private ValuesManager vm = new ValuesManager();

	private GUIParameter wsSettings = null;

	private GUIParameter cmisEnabled = null;

	private GUIParameter cmisChangelog = null;

	private GUIParameter cmisMaxItems = null;

	private GUIParameter wdSettings = null;

	private GUIParameter wdCache = null;

	private GUIParameter convert = null;

	private GUIParameter ghost = null;

	private CheckboxItem extCallParamUser;

	private CheckboxItem extCallParamTitle;

	private GUIParameter openssl = null;

	private GUIParameter pdftohtml = null;

	private GUIParameter keytool = null;

	private GUIParameter ftpEnabled = null;

	private GUIParameter ftpPort = null;

	private GUIParameter ftpSsl = null;

	private GUIParameter ftpKeystoreFile = null;

	private GUIParameter ftpKeystoreAlias = null;

	private GUIParameter ftpKeystorePassword = null;

	public ExternalAppsPanel(GUIParameter[] settings) {
		super("externalapps");

		for (GUIParameter parameter : settings) {
			if (parameter.getName().startsWith("webservice"))
				wsSettings = parameter;
			else if (parameter.getName().equals("cmis.enabled"))
				cmisEnabled = parameter;
			else if (parameter.getName().equals("cmis.changelog"))
				cmisChangelog = parameter;
			else if (parameter.getName().equals("cmis.maxitems"))
				cmisMaxItems = parameter;
			else if (parameter.getName().equals("webdav.enabled"))
				wdSettings = parameter;
			else if (parameter.getName().equals("webdav.usecache"))
				wdCache = parameter;
			else if (parameter.getName().equals("command.convert"))
				convert = parameter;
			else if (parameter.getName().equals("command.gs"))
				ghost = parameter;
			else if (parameter.getName().equals("command.openssl"))
				openssl = parameter;
			else if (parameter.getName().equals("command.pdftohtml"))
				pdftohtml = parameter;
			else if (parameter.getName().equals("command.keytool"))
				keytool = parameter;
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
			else if (parameter.getName().equals("ftp.keystore.password"))
				ftpKeystorePassword = parameter;
		}

		Tab webService = new Tab();
		webService.setTitle(I18N.message("webservice"));

		DynamicForm webServiceForm = new DynamicForm();
		webServiceForm.setValuesManager(vm);
		webServiceForm.setTitleOrientation(TitleOrientation.LEFT);
		webServiceForm.setNumCols(2);
		webServiceForm.setColWidths(1, "*");
		webServiceForm.setPadding(5);

		// Url
		StaticTextItem url = ItemFactory.newStaticTextItem("wsUrl", I18N.message("url"), GWT.getHostPageBaseURL()
				+ "services");

		// Web Service Enabled
		RadioGroupItem wsEnabled = ItemFactory.newBooleanSelector("wsEnabled", "enabled");
		wsEnabled.setName("wsEnabled");
		wsEnabled.setRequired(true);
		wsEnabled.setValue(wsSettings.getValue().equals("true") ? "yes" : "no");

		if (Session.get().isDefaultTenant())
			webServiceForm.setItems(url, wsEnabled);
		else
			webServiceForm.setItems(url);
		webService.setPane(webServiceForm);

		Tab cmis = new Tab();
		cmis.setTitle("CMIS");

		DynamicForm cmisForm = new DynamicForm();
		cmisForm.setValuesManager(vm);
		cmisForm.setTitleOrientation(TitleOrientation.LEFT);
		cmisForm.setNumCols(2);
		cmisForm.setColWidths(1, "*");
		cmisForm.setPadding(5);

		// Url
		StaticTextItem cmisUrl = ItemFactory.newStaticTextItem("cmisUrl", I18N.message("url"), GWT.getHostPageBaseURL()
				+ "service/cmis");

		// CMIS Service Enabled
		RadioGroupItem cmisEnabledItem = ItemFactory.newBooleanSelector("cmisEnabled", "enabled");
		cmisEnabledItem.setRequired(true);
		cmisEnabledItem.setWrapTitle(false);
		cmisEnabledItem.setValue(cmisEnabled.getValue().equals("true") ? "yes" : "no");

		// CMIS Changelog
		RadioGroupItem cmisChangelogItem = ItemFactory.newBooleanSelector("cmisChangelog", "changelog");
		cmisChangelogItem.setRequired(true);
		cmisChangelogItem.setWrapTitle(false);
		cmisChangelogItem.setValue(cmisChangelog.getValue().equals("true") ? "yes" : "no");

		// CMIS Max Items
		IntegerItem cmisMaxItemsItem = ItemFactory.newIntegerItem("cmisMaxItems", "maxitems",
				Integer.parseInt(cmisMaxItems.getValue()));
		cmisMaxItemsItem.setRequired(true);
		cmisMaxItemsItem.setWrapTitle(false);

		if (Session.get().isDefaultTenant())
			cmisForm.setItems(cmisUrl, cmisEnabledItem, cmisChangelogItem, cmisMaxItemsItem);
		else
			cmisForm.setItems(cmisUrl);
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
		StaticTextItem wdUrl = ItemFactory.newStaticTextItem("wdUrl", "WebDAV", GWT.getHostPageBaseURL()
				+ "webdav/store");

		StaticTextItem wdbUrl = ItemFactory.newStaticTextItem("wdbUrl", "WebDAV basic", GWT.getHostPageBaseURL()
				+ "webdavb");
		wdbUrl.setVisible(Feature.enabled(Feature.WEBDAV_BASIC));
		wdbUrl.setHint("&nbsp;&nbsp;&nbsp;(" + I18N.message("webdavbtooltip") + ")");

		// Status
		RadioGroupItem wdEnabled = ItemFactory.newBooleanSelector("wdEnabled", "enabled");
		wdEnabled.setName("wdEnabled");
		wdEnabled.setRequired(true);
		wdEnabled.setValue(wdSettings.getValue().equals("true") ? "yes" : "no");

		// Webdav Cache
		RadioGroupItem cache = ItemFactory.newBooleanSelector("wdCache", "usecache");
		cache.setName("wdCache");
		cache.setRequired(true);
		cache.setWrap(false);
		cache.setWrapTitle(false);
		cache.setValue(wdCache.getValue().equals("true") ? "yes" : "no");

		if (Session.get().isDefaultTenant())
			webDavForm.setItems(wdUrl, wdbUrl, wdEnabled, cache);
		else
			webDavForm.setItems(wdUrl, wdbUrl);
		webDav.setPane(webDavForm);

		// FTP Service status
		RadioGroupItem ftpEnabledItem = ItemFactory.newBooleanSelector("ftpEnabled", "enabled");
		ftpEnabledItem.setRequired(true);
		ftpEnabledItem.setWrapTitle(false);
		ftpEnabledItem.setValue(ftpEnabled.getValue().equals("true") ? "yes" : "no");
		ftpEnabledItem.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				onFtpChanges();
			}
		});

		// FTP port
		IntegerItem ftpPortItem = ItemFactory.newIntegerItem("ftpPort", "port", 21);
		ftpPortItem.setRequired(true);
		ftpPortItem.setWrapTitle(false);
		ftpPortItem.setWidth(80);
		ftpPortItem.setValue(Integer.parseInt(ftpPort.getValue()));
		ftpPortItem.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				onFtpChanges();
			}
		});

		// FTP security
		RadioGroupItem ftpSslItem = ItemFactory.newBooleanSelector("ftpSsl", "encryptionftps");
		ftpSslItem.setRequired(true);
		ftpSslItem.setWrapTitle(false);
		ftpSslItem.setValue(ftpSsl.getValue().equals("true") ? "yes" : "no");
		ftpSslItem.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				onFtpChanges();
			}
		});
		TextItem ftpKeystoreFileItem = ItemFactory.newTextItem("ftpKeystoreFile", "keystore",
				ftpKeystoreFile.getValue());
		ftpKeystoreFileItem.setRequired(false);
		ftpKeystoreFileItem.setWrapTitle(false);
		ftpKeystoreFileItem.setWidth(400);
		ftpKeystoreFileItem.setValue(ftpKeystoreFile.getValue());
		ftpKeystoreFileItem.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				onFtpChanges();
			}
		});
		TextItem ftpKeystoreAliasItem = ItemFactory.newTextItem("ftpKeystoreAlias", "keystorealias",
				ftpKeystoreAlias.getValue());
		ftpKeystoreAliasItem.setRequired(false);
		ftpKeystoreAliasItem.setWrapTitle(false);
		ftpKeystoreAliasItem.setValue(ftpKeystoreAlias.getValue());
		ftpKeystoreAliasItem.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				onFtpChanges();
			}
		});
		TextItem ftpKeystorePasswordItem = ItemFactory.newTextItem("ftpKeystorePassword", "keystorepassword",
				ftpKeystorePassword.getValue());
		ftpKeystorePasswordItem.setRequired(false);
		ftpKeystorePasswordItem.setWrapTitle(false);
		ftpKeystorePasswordItem.setValue(ftpKeystorePassword.getValue());
		ftpKeystorePasswordItem.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				onFtpChanges();
			}
		});

		Tab ftp = new Tab();
		ftp.setTitle(I18N.message("fftp"));
		DynamicForm ftpForm = new DynamicForm();
		ftpForm.setValuesManager(vm);
		ftpForm.setTitleOrientation(TitleOrientation.LEFT);
		ftpForm.setNumCols(2);
		ftpForm.setColWidths(1, "*");
		ftpForm.setPadding(5);
		ftpForm.setItems(ftpEnabledItem, ftpPortItem, ftpSslItem, ftpKeystoreFileItem, ftpKeystoreAliasItem,
				ftpKeystorePasswordItem);
		ftp.setPane(ftpForm);

		DynamicForm extAppForm = new DynamicForm();
		extAppForm.setValuesManager(vm);
		extAppForm.setTitleOrientation(TitleOrientation.LEFT);
		extAppForm.setPadding(5);

		TextItem convertCommand = ItemFactory.newTextItem("convertCommand", "Convert", convert.getValue());
		convertCommand.setWidth(400);
		TextItem ghostCommand = ItemFactory.newTextItem("ghostCommand", "Ghostscript", ghost.getValue());
		ghostCommand.setWidth(400);
		TextItem pdftohtmlCommand = ItemFactory.newTextItem("pdftohtmlCommand", "Pdftohtml", pdftohtml.getValue());
		pdftohtmlCommand.setWidth(400);
		TextItem opensslCommand = ItemFactory.newTextItem("opensslCommand", "OpenSSL", openssl.getValue());
		opensslCommand.setWidth(400);
		TextItem keytoolCommand = ItemFactory.newTextItem("keytoolCommand", "Keytool", keytool.getValue());
		keytoolCommand.setWidth(400);

		extAppForm.setItems(convertCommand, ghostCommand, pdftohtmlCommand, opensslCommand, keytoolCommand);

		if (Session.get().isDefaultTenant()) {
			body.setMembers(extAppForm);
		} else
			tab.setDisabled(true);

		// External Call
		Tab extCall = prepareExternalCall(settings);

		if (Feature.visible(Feature.WEBSERVICE)) {
			tabs.addTab(webService);
			if (!Feature.enabled(Feature.WEBSERVICE))
				webService.setPane(new FeatureDisabled());
		}

		if (Feature.visible(Feature.CMIS)) {
			tabs.addTab(cmis);
			if (!Feature.enabled(Feature.CMIS))
				cmis.setPane(new FeatureDisabled());
		}

		if (Feature.visible(Feature.WEBDAV)) {
			tabs.addTab(webDav);
			if (!Feature.enabled(Feature.WEBDAV))
				webDav.setPane(new FeatureDisabled());
		}

		if (Feature.visible(Feature.FTP)) {
			tabs.addTab(ftp);
			if (!Feature.enabled(Feature.FTP))
				ftp.setPane(new FeatureDisabled());
		}

		if (Feature.visible(Feature.EXTERNAL_CALL)) {
			tabs.addTab(extCall);
			if (!Feature.enabled(Feature.EXTERNAL_CALL))
				extCall.setPane(new FeatureDisabled());
		}

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@SuppressWarnings("unchecked")
			public void onClick(ClickEvent event) {
				Map<String, Object> values = (Map<String, Object>) vm.getValues();

				if (vm.validate()) {
					if (Session.get().isDefaultTenant()) {
						ExternalAppsPanel.this.wsSettings.setValue(values.get("wsEnabled").equals("yes") ? "true"
								: "false");

						ExternalAppsPanel.this.cmisEnabled.setValue(values.get("cmisEnabled").equals("yes") ? "true"
								: "false");

						ExternalAppsPanel.this.cmisChangelog
								.setValue(values.get("cmisChangelog").equals("yes") ? "true" : "false");

						ExternalAppsPanel.this.cmisMaxItems.setValue(values.get("cmisMaxItems").toString());

						ExternalAppsPanel.this.wdSettings.setValue(values.get("wdEnabled").equals("yes") ? "true"
								: "false");

						ExternalAppsPanel.this.wdCache.setValue(values.get("wdCache").equals("yes") ? "true" : "false");
						ExternalAppsPanel.this.convert.setValue(values.get("convertCommand").toString());
						ExternalAppsPanel.this.ghost.setValue(values.get("ghostCommand").toString());
						ExternalAppsPanel.this.openssl.setValue(values.get("opensslCommand").toString());
						ExternalAppsPanel.this.pdftohtml.setValue(values.get("pdftohtmlCommand").toString());
						ExternalAppsPanel.this.keytool.setValue(values.get("keytoolCommand").toString());

						ExternalAppsPanel.this.ftpEnabled.setValue(values.get("ftpEnabled").equals("yes") ? "true"
								: "false");
						ExternalAppsPanel.this.ftpPort.setValue(values.get("ftpPort").toString());
						ExternalAppsPanel.this.ftpSsl.setValue(values.get("ftpSsl").equals("yes") ? "true"
								: "false");
						ExternalAppsPanel.this.ftpKeystoreFile.setValue(values.get("ftpKeystoreFile").toString());
						ExternalAppsPanel.this.ftpKeystoreAlias.setValue(values.get("ftpKeystoreAlias").toString());
						ExternalAppsPanel.this.ftpKeystorePassword.setValue(values.get("ftpKeystorePassword").toString());
					}

					List<GUIParameter> params = new ArrayList<GUIParameter>();
					params.add(ExternalAppsPanel.this.wsSettings);
					params.add(ExternalAppsPanel.this.wdSettings);
					params.add(ExternalAppsPanel.this.wdCache);
					params.add(ExternalAppsPanel.this.convert);
					params.add(ExternalAppsPanel.this.ghost);
					params.add(ExternalAppsPanel.this.openssl);
					params.add(ExternalAppsPanel.this.pdftohtml);
					params.add(ExternalAppsPanel.this.keytool);
					params.add(ExternalAppsPanel.this.cmisEnabled);
					params.add(ExternalAppsPanel.this.cmisChangelog);
					params.add(ExternalAppsPanel.this.cmisMaxItems);
					params.add(ExternalAppsPanel.this.ftpEnabled);
					params.add(ExternalAppsPanel.this.ftpPort);
					params.add(ExternalAppsPanel.this.ftpSsl);
					params.add(ExternalAppsPanel.this.ftpKeystoreFile);
					params.add(ExternalAppsPanel.this.ftpKeystoreAlias);
					params.add(ExternalAppsPanel.this.ftpKeystorePassword);

					// External Call
					try {
						GUIExternalCall extCall = new GUIExternalCall();
						extCall.setName(values.get("extCallName") == null ? "" : values.get("extCallName").toString());
						extCall.setBaseUrl(values.get("extCallBaseUrl") == null ? "" : values.get("extCallBaseUrl")
								.toString());
						extCall.setSuffix(values.get("extCallSuffix") == null ? "" : values.get("extCallSuffix")
								.toString());
						extCall.setTargetWindow(values.get("extCallWindow") == null ? "" : values.get("extCallWindow")
								.toString());
						if ("yes".equals(values.get("extCallEnabled")))
							Session.get().getSession().setExternalCall(extCall);
						else
							Session.get().getSession().setExternalCall(null);

						String tenant = Session.get().getTenantName();
						params.add(new GUIParameter(tenant + ".extcall.enabled", "yes".equals(values
								.get("extCallEnabled")) ? "true" : "false"));
						params.add(new GUIParameter(tenant + ".extcall.name", extCall.getName()));
						params.add(new GUIParameter(tenant + ".extcall.baseurl", extCall.getBaseUrl()));
						params.add(new GUIParameter(tenant + ".extcall.suffix", extCall.getSuffix()));
						params.add(new GUIParameter(tenant + ".extcall.window", extCall.getTargetWindow()));

						ArrayList<String> buf = new ArrayList<String>();
						if (extCallParamUser.getValueAsBoolean() != null
								&& extCallParamUser.getValueAsBoolean().booleanValue())
							buf.add("user");
						if (extCallParamTitle.getValueAsBoolean() != null
								&& extCallParamTitle.getValueAsBoolean().booleanValue())
							buf.add("filename");
						String paramsStr = buf.toString().substring(1, buf.toString().length() - 1);

						extCall.setParametersStr(paramsStr);
						params.add(new GUIParameter(tenant + ".extcall.params", buf.isEmpty() ? "" : paramsStr));
					} catch (Throwable t) {
					}

					SettingService.Instance.get().saveSettings(params.toArray(new GUIParameter[0]),
							new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									Log.serverError(caught);
								}

								@Override
								public void onSuccess(Void ret) {
									Log.info(I18N.message("settingssaved"), null);
								}
							});
				} else {
					SC.warn(I18N.message("invalidsettings"));
				}
			}
		});

		addMember(save);

		if (Session.get().isDemo()) {
			// In demo mode you cannot alter the client configurations
			save.setDisabled(true);
		}
	}

	protected void onFtpChanges() {
		ToastNotification.showNotification(I18N.message("needrestart"));
	}

	private Tab prepareExternalCall(GUIParameter[] settings) {
		VLayout pane = new VLayout();

		Tab extCall = new Tab();
		extCall.setTitle(I18N.message("externalcall"));
		DynamicForm extCallForm = new DynamicForm();
		extCallForm.setWidth(400);
		extCallForm.setIsGroup(true);
		extCallForm.setNumCols(2);
		extCallForm.setPadding(2);
		extCallForm.setGroupTitle(I18N.message("externalcall"));
		extCallForm.setValuesManager(vm);
		extCallForm.setTitleOrientation(TitleOrientation.LEFT);
		final RadioGroupItem extCallEnabled = ItemFactory.newBooleanSelector("extCallEnabled", "enabled");
		extCallEnabled.setRequired(true);
		extCallEnabled.setRedrawOnChange(true);
		extCallEnabled.setValue("no");

		TextItem extCallName = ItemFactory.newTextItem("extCallName", I18N.message("name"), null);
		TextItem extCallBaseUrl = ItemFactory.newTextItem("extCallBaseUrl", I18N.message("baseurl"), null);
		extCallBaseUrl.setWidth(300);
		TextItem extCallSuffix = ItemFactory.newTextItem("extCallSuffix", I18N.message("suffix"), null);
		extCallSuffix.setWidth(300);
		TextItem extCallWindow = ItemFactory.newTextItem("extCallWindow", I18N.message("targetwindow"), "_blank");

		extCallForm.setItems(extCallEnabled, extCallName, extCallBaseUrl, extCallSuffix, extCallWindow);

		// Use a second form to group the parameters section
		DynamicForm parametersForm = new DynamicForm();
		parametersForm.setWidth(400);
		parametersForm.setIsGroup(true);
		parametersForm.setGroupTitle(I18N.message("parameters"));
		parametersForm.setNumCols(4);
		extCallForm.setPadding(2);
		parametersForm.setValuesManager(vm);
		extCallParamUser = ItemFactory.newCheckbox("extCallParamUser", "user");
		extCallParamTitle = ItemFactory.newCheckbox("extCallParamTitle", "filename");
		parametersForm.setItems(extCallParamUser, extCallParamTitle);

		pane.setMembers(extCallForm, parametersForm);
		extCall.setPane(pane);

		String tenant = Session.get().getTenantName();
		for (GUIParameter s : settings) {
			if ((tenant + ".extcall.enabled").equals(s.getName()))
				extCallEnabled.setValue("true".equals(s.getValue()) ? "yes" : "no");
			else if ((tenant + ".extcall.name").equals(s.getName()))
				extCallName.setValue(s.getValue());
			else if ((tenant + ".extcall.baseurl").equals(s.getName()))
				extCallBaseUrl.setValue(s.getValue());
			else if ((tenant + ".extcall.suffix").equals(s.getName()))
				extCallSuffix.setValue(s.getValue());
			else if ((tenant + ".extcall.window").equals(s.getName()))
				extCallWindow.setValue(s.getValue());
			else if ((tenant + ".extcall.params").equals(s.getName())) {
				String[] tokens = s.getValue().split(",");
				for (String param : tokens) {
					if ("user".equals(param.trim()))
						extCallParamUser.setValue("true");
					else if ("filename".equals(param.trim()))
						extCallParamTitle.setValue("true");
				}
			}
		}

		return extCall;
	}
}