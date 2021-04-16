package com.logicaldoc.gui.frontend.client.settings.gui;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel shows the Web Service and WebDAV settings, and also the external
 * applications.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUISettingsPanel extends AdminPanel {

	private ValuesManager vm = new ValuesManager();

	public GUISettingsPanel() {
		super("guisettings");
	}

	@Override
	protected void onDraw() {
		SettingService.Instance.get().loadGUISettings(new AsyncCallback<GUIParameter[]>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIParameter[] settings) {
				initGUI(settings);
			}
		});
	}

	private void initGUI(GUIParameter[] settings) {
		DynamicForm parametersForm = new DynamicForm();
		parametersForm.setValuesManager(vm);
		parametersForm.setTitleOrientation(TitleOrientation.LEFT);
		parametersForm.setNumCols(4);
		parametersForm.setPadding(5);
		parametersForm.setAutoWidth();

		body.setMembers(parametersForm);

		Tab languages = new Tab();
		languages.setTitle(I18N.message("languages"));
		languages.setPane(new GUILanguagesPanel());

		Tab grids = new Tab();
		grids.setTitle(I18N.message("grids"));
		grids.setPane(new GUIGridsPanel());
		tabs.addTab(grids);

		Tab dashlets = new Tab();
		dashlets.setTitle(I18N.message("dashlets"));
		dashlets.setPane(new DashletsPanel());
		tabs.addTab(dashlets);

		Tab customActions = new Tab();
		customActions.setTitle(I18N.message("customactions"));
		customActions.setPane(new CustomActionsPanel());
		
		if (Feature.visible(Feature.CUSTOM_ACTIONS)) {
			tabs.addTab(customActions);
			if (!Feature.enabled(Feature.CUSTOM_ACTIONS)) {
				customActions.setPane(new FeatureDisabled());
			}
		}
		
		if (Feature.visible(Feature.GUI_LANGUAGES)) {
			tabs.addTab(languages);
			if (!Feature.enabled(Feature.GUI_LANGUAGES)) {
				languages.setPane(new FeatureDisabled());
			}
		}

		TextItem welcome = ItemFactory.newTextItemForAutomation("welcome", I18N.message("welcomemessage"), null, null);
		welcome.setWidth(400);

		SelectItem density = ItemFactory.newDensitySelector();

		RadioGroupItem saveLogin = ItemFactory.newBooleanSelector("savelogin", I18N.message("savelogin"));
		saveLogin.setHint(I18N.message("saveloginhint"));
		saveLogin.setWrapTitle(false);

		SpinnerItem previewSize = ItemFactory.newSpinnerItem("previewsize", I18N.message("previewwindow"),
				(Integer) null);
		previewSize.setHint("%");
		previewSize.setRequired(true);
		previewSize.setWrapTitle(false);
		previewSize.setMax(100);
		previewSize.setMin(1);
		previewSize.setStep(10);

		SpinnerItem previewTimeout = ItemFactory.newSpinnerItem("previewtimeout", "previewtimeout", (Integer) null);
		previewTimeout.setHint(I18N.message("seconds"));
		previewTimeout.setWrapTitle(false);
		previewTimeout.setRequired(true);
		previewSize.setMin(1);
		previewSize.setStep(10);

		RadioGroupItem banner = ItemFactory.newBooleanSelector("banner",
				I18N.message("banner"));
		banner.setWrapTitle(false);
		
		RadioGroupItem openPreviewPanel = ItemFactory.newBooleanSelector("openpreviewpanel",
				I18N.message("openpreviewpanel"));
		openPreviewPanel.setWrapTitle(false);

		RadioGroupItem reactToRemoteEvents = ItemFactory.newBooleanSelector("reacttoremoteevents",
				I18N.message("reacttoremoteevents"));
		reactToRemoteEvents.setWrapTitle(false);

		RadioGroupItem showLicenseAlertsInLogin = ItemFactory.newBooleanSelector("showlicensealertsinlogin",
				I18N.message("showlicensealertsinlogin"));
		showLicenseAlertsInLogin.setWrapTitle(false);

		RadioGroupItem showQuotaAlertsInLogin = ItemFactory.newBooleanSelector("showquotaalertsinlogin",
				I18N.message("showquotaalertsinlogin"));
		showQuotaAlertsInLogin.setWrapTitle(false);

		RadioGroupItem showUpdateAlertsInLogin = ItemFactory.newBooleanSelector("showupdatealertsinlogin",
				I18N.message("showupdatealertsinlogin"));
		showUpdateAlertsInLogin.setWrapTitle(false);

		RadioGroupItem showPatchAlertsInLogin = ItemFactory.newBooleanSelector("showpatchalertsinlogin",
				I18N.message("showpatchalertsinlogin"));
		showPatchAlertsInLogin.setWrapTitle(false);

		RadioGroupItem showLanguageInLogin = ItemFactory.newBooleanSelector("showlanguageinlogin",
				I18N.message("showlanguageinlogin"));
		showLicenseAlertsInLogin.setWrapTitle(false);

		RadioGroupItem showLostPassword = ItemFactory.newBooleanSelector("showlostpassword",
				I18N.message("showlostpasswordlink"));
		showLostPassword.setWrapTitle(false);

		RadioGroupItem galleryEnabled = ItemFactory.newBooleanSelector("galleryenabled",
				I18N.message("galleryenabled"));
		galleryEnabled.setWrapTitle(false);

		RadioGroupItem saveInputs = ItemFactory.newBooleanSelector("saveinputs", I18N.message("saveinputs"));
		galleryEnabled.setWrapTitle(false);

		SpinnerItem attrTextAreaW = ItemFactory.newSpinnerItem("textareaw", I18N.message("attrtextareaw"),
				(Integer) null);
		attrTextAreaW.setHint("pixels");
		attrTextAreaW.setRequired(true);
		attrTextAreaW.setWrapTitle(false);
		attrTextAreaW.setStep(50);
		attrTextAreaW.setMin(50);

		SpinnerItem attrTextAreaH = ItemFactory.newSpinnerItem("textareah", I18N.message("attrtextareah"),
				(Integer) null);
		attrTextAreaH.setHint("pixels");
		attrTextAreaH.setRequired(true);
		attrTextAreaH.setWrapTitle(false);
		attrTextAreaH.setStep(50);
		attrTextAreaH.setMin(50);

		SpinnerItem attrTextBoxW = ItemFactory.newSpinnerItem("textboxw", I18N.message("attrtextboxw"), (Integer) null);
		attrTextBoxW.setHint("pixels");
		attrTextBoxW.setRequired(true);
		attrTextBoxW.setWrapTitle(false);
		attrTextBoxW.setStep(50);
		attrTextBoxW.setMin(50);

		SpinnerItem noteMaxSize = ItemFactory.newSpinnerItem("notemaxsize", I18N.message("notemaxsize"),
				(Integer) null);
		noteMaxSize.setHint(I18N.message("chars").toLowerCase());
		noteMaxSize.setRequired(true);
		noteMaxSize.setWrapTitle(false);
		noteMaxSize.setStep(100);
		noteMaxSize.setMin(0);
		noteMaxSize.setWidth(70);

		SpinnerItem emailMaxSize = ItemFactory.newSpinnerItem("emailmaxsize", I18N.message("emailmaxsize"),
				(Integer) null);
		emailMaxSize.setHint(I18N.message("chars").toLowerCase());
		emailMaxSize.setRequired(true);
		emailMaxSize.setWrapTitle(false);
		emailMaxSize.setStep(100);
		emailMaxSize.setMin(0);
		emailMaxSize.setWidth(70);

		SpinnerItem thumbSize = ItemFactory.newSpinnerItem("thumbsize", I18N.message("thumbsize"), (Integer) null);
		thumbSize.setHint("pixels");
		thumbSize.setRequired(true);
		thumbSize.setWrapTitle(false);
		thumbSize.setMin(10);
		thumbSize.setStep(10);

		SpinnerItem thumbQuality = ItemFactory.newSpinnerItem("thumbquality", I18N.message("thumbquality"),
				(Integer) null);
		thumbQuality.setHint("%");
		thumbQuality.setRequired(true);
		thumbQuality.setWrapTitle(false);
		thumbQuality.setMin(1);
		thumbQuality.setStep(10);
		thumbQuality.setMax(100);

		SpinnerItem mobileSize = ItemFactory.newSpinnerItem("mobilesize", I18N.message("mobilesize"), (Integer) null);
		mobileSize.setHint("pixels");
		mobileSize.setRequired(true);
		mobileSize.setWrapTitle(false);
		mobileSize.setMin(10);
		mobileSize.setStep(10);

		SpinnerItem mobileQuality = ItemFactory.newSpinnerItem("mobilequality", I18N.message("mobilequality"),
				(Integer) null);
		mobileQuality.setHint("%");
		mobileQuality.setRequired(true);
		mobileQuality.setWrapTitle(false);
		mobileQuality.setMin(1);
		mobileQuality.setStep(10);

		SpinnerItem tileSize = ItemFactory.newSpinnerItem("tilesize", I18N.message("tilesize"), (Integer) null);
		tileSize.setHint("pixels");
		tileSize.setRequired(true);
		tileSize.setMin(1);
		tileSize.setStep(10);

		SpinnerItem tileQuality = ItemFactory.newSpinnerItem("tilequality", I18N.message("tilequality"),
				(Integer) null);
		tileQuality.setHint("%");
		tileQuality.setRequired(true);
		tileQuality.setWrapTitle(false);
		tileQuality.setMin(1);
		tileQuality.setStep(10);
		tileQuality.setMax(100);

		SpinnerItem uploadMax = ItemFactory.newSpinnerItem("uploadmax", I18N.message("uploadmax"), (Integer) null);
		uploadMax.setHint("MB");
		uploadMax.setRequired(true);
		uploadMax.setWrapTitle(false);
		uploadMax.setMin(0);
		uploadMax.setStep(10);
		uploadMax.setWidth(70);

		SpinnerItem previewMaxFileSize = ItemFactory.newSpinnerItem("previewmaxfilesize",
				I18N.message("previewmaxfilesize"), (Integer) null);
		previewMaxFileSize.setHint("MB");
		previewMaxFileSize.setRequired(true);
		previewMaxFileSize.setWrapTitle(false);
		previewMaxFileSize.setMin(0);
		previewMaxFileSize.setStep(1);

		TextItem disallow = ItemFactory.newTextItem("disallow", I18N.message("disallowedext"), null);
		disallow.setHint(I18N.message("separatedcomma"));
		disallow.setWidth(400);
		disallow.setRequired(false);
		disallow.setWrapTitle(false);

		TextItem textExtensions = ItemFactory.newTextItem("textextensions", I18N.message("textextensions"), null);
		textExtensions.setHint(I18N.message("separatedcomma"));
		textExtensions.setWidth(400);
		textExtensions.setRequired(false);
		textExtensions.setWrapTitle(false);

		RadioGroupItem ondoubleclick = ItemFactory.newBooleanSelector("ondoubleclick", "ondoubleclick");
		ondoubleclick.setValueMap("download", "preview");

		RadioGroupItem docTab = ItemFactory.newBooleanSelector("doctab", "doctab");
		docTab.setValueMap("properties", "preview");

		RadioGroupItem foldSorting = ItemFactory.newBooleanSelector("foldsorting", "foldsorting");
		foldSorting.setValueMap("name", "date");

		RadioGroupItem downloadTicketBehavior = ItemFactory.newBooleanSelector("downloadticketbehavior",
				I18N.message("downloadticketbehavior"));
		downloadTicketBehavior.setValueMap("download", "display");

		RadioGroupItem webstartMode = ItemFactory.newBooleanSelector("webstartmode", I18N.message("webstartmode"));
		webstartMode.setValueMap("webstart", "download");

		RadioGroupItem foldOpentree = ItemFactory.newBooleanSelector("foldopentree", I18N.message("openfolderstree"));
		foldOpentree.setWrapTitle(false);

		SpinnerItem foldMaxChildren = ItemFactory.newSpinnerItem("foldmaxchildren", I18N.message("foldmaxchildren"),
				(Integer) null);
		foldMaxChildren.setWrapTitle(false);

		SpinnerItem maxHistories = ItemFactory.newSpinnerItem("maxhistories", I18N.message("maxhistories"),
				(Integer) null);
		maxHistories.setStep(10);
		maxHistories.setWrapTitle(false);

		RadioGroupItem foldPagination = ItemFactory.newBooleanSelector("foldpagination",
				I18N.message("foldpagination"));
		foldPagination.setWrapTitle(false);

		RadioGroupItem autocloseFolderNodes = ItemFactory.newBooleanSelector("autoclosefoldernodes",
				I18N.message("autoclosefoldernodes"));
		autocloseFolderNodes.setWrapTitle(false);

		RadioGroupItem inheritSecurityOption = ItemFactory.newBooleanSelector("inheritsecurityoption",
				I18N.message("inheritsecurityoption"));
		inheritSecurityOption.setWrapTitle(false);

		RadioGroupItem inheritSecurityOptionDefault = ItemFactory.newBooleanSelector("inheritsecurityoptiondef",
				I18N.message("inheritsecurityoptiondef"));
		inheritSecurityOptionDefault.setWrapTitle(false);

		TextItem webcontentFolders = ItemFactory.newTextItem("webcontentfolders", I18N.message("webcontentfolders"),
				null);
		webcontentFolders.setHint(I18N.message("commaseplistofids"));
		webcontentFolders.setWidth(400);

		SpinnerItem sessionTimeout = ItemFactory.newSpinnerItem("sessiontimeout", I18N.message("sessiontimeout"),
				(Integer) null);
		sessionTimeout.setHint(I18N.message("minutes"));
		sessionTimeout.setRequired(true);
		sessionTimeout.setWrapTitle(false);
		sessionTimeout.setMin(1);
		sessionTimeout.setStep(5);

		SpinnerItem rpcTimeout = ItemFactory.newSpinnerItem("rpctimeout", I18N.message("rpctimeout"), (Integer) null);
		rpcTimeout.setHint(I18N.message("minutes"));
		rpcTimeout.setRequired(true);
		rpcTimeout.setWrapTitle(false);
		rpcTimeout.setMin(1);
		rpcTimeout.setStep(1);

		SpinnerItem sessionHeartbeat = ItemFactory.newSpinnerItem("sessionheartbeat", I18N.message("sessionheartbeat"),
				(Integer) null);
		sessionHeartbeat.setHint(I18N.message("seconds"));
		sessionHeartbeat.setRequired(true);
		sessionHeartbeat.setWrapTitle(false);
		sessionHeartbeat.setMin(0);
		sessionHeartbeat.setStep(10);

		SpinnerItem popupTimeout = ItemFactory.newSpinnerItem("popuptimeout", I18N.message("popuptimeout"),
				(Integer) null);
		popupTimeout.setHint(I18N.message("seconds"));
		popupTimeout.setRequired(true);
		popupTimeout.setWrapTitle(false);
		popupTimeout.setMin(1);
		popupTimeout.setStep(1);

		RadioGroupItem askVersionCommentOnSave = ItemFactory.newBooleanSelector("askversioncommentonsave",
				I18N.message("askversioncommentonsave"));
		askVersionCommentOnSave.setWrapTitle(false);
		
		SpinnerItem avatarSize = ItemFactory.newSpinnerItem("avatarsize", I18N.message("avatarsize"), (Integer) null);
		avatarSize.setHint("pixels");
		avatarSize.setRequired(true);
		avatarSize.setWrapTitle(false);
		avatarSize.setStep(16);
		avatarSize.setMin(16);

		for (GUIParameter p : settings) {
			if (p.getName().endsWith("gui.banner"))
				banner.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.welcome"))
				welcome.setValue(p.getValue());
			if (p.getName().endsWith("gui.savelogin"))
				saveLogin.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.preview.size"))
				previewSize.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.preview.timeout"))
				previewTimeout.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.preview.openpanel"))
				openPreviewPanel.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.serverpush"))
				reactToRemoteEvents.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.license.showloginalerts"))
				showLicenseAlertsInLogin.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.quota.showloginalerts"))
				showQuotaAlertsInLogin.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.update.showloginalerts"))
				showUpdateAlertsInLogin.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.patch.showloginalerts"))
				showPatchAlertsInLogin.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.login.lang"))
				showLanguageInLogin.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.lostpassword.show"))
				showLostPassword.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.thumbnail.size"))
				thumbSize.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.thumbnail.quality"))
				thumbQuality.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.mobile.size"))
				mobileSize.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.mobile.quality"))
				mobileQuality.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.textarea.w"))
				attrTextAreaW.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.textarea.h"))
				attrTextAreaH.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.textbox.w"))
				attrTextBoxW.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.note.maxlength"))
				noteMaxSize.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.email.maxlength"))
				emailMaxSize.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.tile.size"))
				tileSize.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.tile.quality"))
				tileQuality.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.doubleclick"))
				ondoubleclick.setValue(p.getValue());
			if (p.getName().endsWith("gui.document.tab"))
				docTab.setValue(p.getValue());
			if (p.getName().endsWith("gui.folder.sorting"))
				foldSorting.setValue(p.getValue());
			if (p.getName().endsWith("gui.folder.maxchildren"))
				foldMaxChildren.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.maxhistories"))
				maxHistories.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.folder.autoclose"))
				autocloseFolderNodes.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.folder.pagination"))
				foldPagination.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.folder.opentree"))
				foldOpentree.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.text.extensions") && p.getValue() != null)
				textExtensions.setValue(p.getValue().trim());
			if (p.getName().endsWith("gui.density") && p.getValue() != null)
				density.setValue(p.getValue().trim());
			if (p.getName().endsWith("upload.maxsize"))
				uploadMax.setValue(Long.parseLong(p.getValue().trim()));
			if (p.getName().endsWith("upload.disallow") && p.getValue() != null)
				disallow.setValue(p.getValue().trim());
			if (p.getName().endsWith("gui.webcontent.folders"))
				webcontentFolders.setValue(p.getValue());
			if (p.getName().endsWith("gui.rpc.timeout"))
				rpcTimeout.setValue(p.getValue());
			if (p.getName().endsWith("gui.popup.timeout"))
				popupTimeout.setValue(p.getValue());
			if (p.getName().endsWith("session.timeout"))
				sessionTimeout.setValue(p.getValue());
			if (p.getName().endsWith("session.heartbeat"))
				sessionHeartbeat.setValue(p.getValue());
			if (p.getName().endsWith("security.inheritoption"))
				inheritSecurityOption.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("security.inheritoption.default"))
				inheritSecurityOptionDefault.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("security.serverpush"))
				reactToRemoteEvents.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("downloadticket.behavior"))
				downloadTicketBehavior.setValue(p.getValue());
			if (p.getName().endsWith("gui.webstart.mode"))
				webstartMode.setValue(p.getValue());
			if (p.getName().endsWith("gui.onsave.askversioncomment".toLowerCase()))
				askVersionCommentOnSave.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.galleryenabled".toLowerCase()))
				galleryEnabled.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.preview.maxfilesize"))
				previewMaxFileSize.setValue(Integer.parseInt(p.getValue().trim()));
			if (p.getName().endsWith("gui.saveinputs"))
				saveInputs.setValue(p.getValue().equals("true") ? "yes" : "no");
			if (p.getName().endsWith("gui.avatar.size"))
				avatarSize.setValue(Long.parseLong(p.getValue().trim()));

		}

		ButtonItem save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			@SuppressWarnings("unchecked")
			public void onClick(ClickEvent event) {
				Map<String, Object> values = (Map<String, Object>) vm.getValues();

				if (vm.validate()) {
					List<GUIParameter> params = new ArrayList<GUIParameter>();
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.banner",
							"yes".equals(values.get("banner")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.welcome",
							(String) values.get("welcome")));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.density",
							(String) values.get("density")));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.savelogin",
							"yes".equals(values.get("savelogin")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.preview.size",
							values.get("previewsize").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.preview.timeout",
							values.get("previewtimeout").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.rpc.timeout",
							values.get("rpctimeout").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.popup.timeout",
							values.get("popuptimeout").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.textarea.w",
							values.get("textareaw").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.textarea.h",
							values.get("textareah").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.textbox.w",
							values.get("textboxw").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.preview.openpanel",
							"yes".equals(values.get("openpreviewpanel")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.thumbnail.size",
							values.get("thumbsize").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.thumbnail.quality",
							values.get("thumbquality").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.mobile.size",
							values.get("mobilesize").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.mobile.quality",
							values.get("mobilequality").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.tile.size",
							values.get("tilesize").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.tile.quality",
							values.get("tilequality").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.note.maxlength",
							values.get("notemaxsize").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.email.maxlength",
							values.get("emailmaxsize").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.doubleclick",
							values.get("ondoubleclick").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.document.tab",
							values.get("doctab").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.sorting",
							values.get("foldsorting").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.opentree",
							"yes".equals(values.get("foldopentree")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.pagination",
							"yes".equals(values.get("foldpagination")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.maxchildren",
							values.get("foldmaxchildren").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.maxhistories",
							values.get("maxhistories").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.autoclose",
							"yes".equals(values.get("autoclosefoldernodes")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.serverpush",
							"yes".equals(values.get("reacttoremoteevents")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.license.showloginalerts",
							"yes".equals(values.get("showlicensealertsinlogin")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.quota.showloginalerts",
							"yes".equals(values.get("showquotaalertsinlogin")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.update.showloginalerts",
							"yes".equals(values.get("showupdatealertsinlogin")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.patch.showloginalerts",
							"yes".equals(values.get("showpatchalertsinlogin")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.security.inheritoption",
							"yes".equals(values.get("inheritsecurityoption")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.security.inheritoption.default",
							"yes".equals(values.get("inheritsecurityoptiondef")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.login.lang",
							"yes".equals(values.get("showlanguageinlogin")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.lostpassword.show",
							"yes".equals(values.get("showlostpassword")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".upload.maxsize",
							values.get("uploadmax").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".upload.disallow",
							values.get("disallow").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.webcontent.folders",
							values.get("webcontentfolders").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".session.timeout",
							values.get("sessiontimeout").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".session.heartbeat",
							values.get("sessionheartbeat").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".downloadticket.behavior",
							values.get("downloadticketbehavior").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.onsave.askversioncomment",
							"yes".equals(values.get("askversioncommentonsave")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.galleryenabled",
							"yes".equals(values.get("galleryenabled")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.preview.maxfilesize",
							values.get("previewmaxfilesize").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.webstart.mode",
							values.get("webstartmode").toString()));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.saveinputs",
							"yes".equals(values.get("saveinputs")) ? "true" : "false"));
					params.add(new GUIParameter(Session.get().getTenantName() + ".gui.avatar.size",
							values.get("avatarsize").toString()));

					// Update the current session parameters.
					for (GUIParameter p : params)
						Session.get().getInfo().setConfig(p.getName(), p.getValue());

					SettingService.Instance.get().saveSettings(params.toArray(new GUIParameter[0]),
							new AsyncCallback<Void>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Void ret) {
									GuiLog.info(I18N.message("settingssaved"), null);
								}
							});
				}
			}
		});

		parametersForm.setItems(welcome, density, banner, previewSize, previewTimeout, previewMaxFileSize, uploadMax, thumbSize,
				thumbQuality, tileSize, tileQuality, mobileSize, mobileQuality, avatarSize, disallow, textExtensions,
				attrTextBoxW, attrTextAreaW, attrTextAreaH, noteMaxSize, emailMaxSize, ondoubleclick, docTab,
				foldSorting, inheritSecurityOption, inheritSecurityOptionDefault, foldOpentree, foldPagination,
				foldMaxChildren, openPreviewPanel, maxHistories, autocloseFolderNodes, webstartMode, galleryEnabled,
				webcontentFolders, downloadTicketBehavior, saveLogin, sessionTimeout, rpcTimeout, sessionHeartbeat,
				popupTimeout, askVersionCommentOnSave, reactToRemoteEvents, showLicenseAlertsInLogin,
				showQuotaAlertsInLogin, showUpdateAlertsInLogin, showPatchAlertsInLogin, showLanguageInLogin,
				saveInputs, showLostPassword, save);
	}
}