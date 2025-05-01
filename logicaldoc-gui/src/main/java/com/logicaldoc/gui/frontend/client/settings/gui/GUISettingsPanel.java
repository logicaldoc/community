package com.logicaldoc.gui.frontend.client.settings.gui;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
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
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel shows the Web Service and WebDAV settings, and also the external
 * applications.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class GUISettingsPanel extends AdminPanel {

	private static final String CHARSET = "charset";

	private static final String DOWNLOAD = "download";

	private static final String PIXELS = "pixels";

	private static final String SECONDS = "seconds";

	private ValuesManager vm = new ValuesManager();

	public GUISettingsPanel() {
		super("guisettings");
	}

	@Override
	protected void onDraw() {
		SettingService.Instance.get().loadGUISettings(new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(List<GUIParameter> settings) {
				initGUI(settings);
			}
		});
	}

	private void initGUI(List<GUIParameter> settings) {
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

		TextItem welcome = ItemFactory.newTextItemForAutomation("welcome", I18N.message("welcomemessage"),
				Util.getParameterValue(settings, "gui.welcome"), null);
		welcome.setWidth(400);

		SelectItem density = ItemFactory.newDensitySelector();
		density.setValue(Util.getParameterValue(settings, "gui.density", "dense").trim());

		ToggleItem saveLogin = ItemFactory.newToggleItem("savelogin",
				Util.getParameterValueAsBoolean(settings, "gui.savelogin"));
		saveLogin.setHint(I18N.message("saveloginhint"));
		saveLogin.setWrapTitle(false);
		
		SpinnerItem previewSize = ItemFactory.newSpinnerItem("previewsize", "previewwindow",
				Integer.parseInt(Util.getParameterValue(settings, "gui.preview.size", "100").trim()));
		previewSize.setHint("%");
		previewSize.setRequired(true);
		previewSize.setWrapTitle(false);
		previewSize.setMax(100);
		previewSize.setMin(1);
		previewSize.setStep(10);

		SpinnerItem previewTimeout = ItemFactory.newSpinnerItem("previewtimeout",
				Integer.parseInt(Util.getParameterValue(settings, "gui.preview.timeout", "30").trim()));
		previewTimeout.setHint(I18N.message(SECONDS));
		previewTimeout.setWrapTitle(false);
		previewTimeout.setRequired(true);
		previewSize.setMin(1);
		previewSize.setStep(10);
		
		ToggleItem banner = ItemFactory.newToggleItem("banner",
				Util.getParameterValueAsBoolean(settings, "gui.banner"));
		banner.setWrapTitle(false);

		ToggleItem openPreviewPanel = ItemFactory.newToggleItem("openpreviewpanel",
				Util.getParameterValueAsBoolean(settings, "gui.preview.openpanel"));
		openPreviewPanel.setWrapTitle(false);
		
		ToggleItem showDocAttrsAsLinks = ItemFactory.newToggleItem("showdocattrsaslinks",
				Util.getParameterValueAsBoolean(settings, "gui.showdocattrsaslinks"));
		showDocAttrsAsLinks.setWrapTitle(false);

		ToggleItem reactToRemoteEvents = ItemFactory.newToggleItem("reacttoremoteevents",
				Util.getParameterValueAsBoolean(settings, "gui.serverpush"));
		reactToRemoteEvents.setWrapTitle(false);
		
		ToggleItem showPushErrors = ItemFactory.newToggleItem("reacttoremoteeventsshowerrors",
				Util.getParameterValueAsBoolean(settings, "gui.serverpush.showerror"));
		showPushErrors.setWrapTitle(false);

		ToggleItem showLicenseAlertsInLogin = ItemFactory.newToggleItem("showlicensealertsinlogin",
				Util.getParameterValueAsBoolean(settings, "gui.license.showloginalerts"));
		showLicenseAlertsInLogin.setWrapTitle(false);

		ToggleItem showQuotaAlertsInLogin = ItemFactory.newToggleItem("showquotaalertsinlogin",
				Util.getParameterValueAsBoolean(settings, "gui.quota.showloginalerts"));
		showQuotaAlertsInLogin.setWrapTitle(false);

		ToggleItem showUpdateAlertsInLogin = ItemFactory.newToggleItem("showupdatealertsinlogin",
				Util.getParameterValueAsBoolean(settings, "gui.update.showloginalerts"));
		showUpdateAlertsInLogin.setWrapTitle(false);
		
		ToggleItem showVersionAlertsInLogin = ItemFactory.newToggleItem("showversionalertsinlogin",
				Util.getParameterValueAsBoolean(settings, "gui.version.showloginalerts"));
		showVersionAlertsInLogin.setWrapTitle(false);

		ToggleItem showPatchAlertsInLogin = ItemFactory.newToggleItem("showpatchalertsinlogin",
				Util.getParameterValueAsBoolean(settings, "gui.patch.showloginalerts"));
		showPatchAlertsInLogin.setWrapTitle(false);

		ToggleItem showLanguageInLogin = ItemFactory.newToggleItem("showlanguageinlogin",
				Util.getParameterValueAsBoolean(settings, "gui.login.lang"));
		showLicenseAlertsInLogin.setWrapTitle(false);
		
		ToggleItem showLostPassword = ItemFactory.newToggleItem("showlostpassword",
				I18N.message("showlostpasswordlink"),
				Util.getParameterValueAsBoolean(settings, "gui.lostpassword.show"));
		showLostPassword.setWrapTitle(false);

		ToggleItem galleryEnabled = ItemFactory.newToggleItem("galleryenabled",
				Util.getParameterValueAsBoolean(settings, "gui.galleryenabled"));
		galleryEnabled.setWrapTitle(false);

		ToggleItem allowNotesEditing = ItemFactory.newToggleItem("allownotesediting",
				Util.getParameterValueAsBoolean(settings, "gui.notes.allowedit"));
		allowNotesEditing.setWrapTitle(false);

		ToggleItem saveInputs = ItemFactory.newToggleItem("saveinputs",
				Util.getParameterValueAsBoolean(settings, "gui.saveinputs"));
		galleryEnabled.setWrapTitle(false);
		
		SpinnerItem attrTextAreaW = ItemFactory.newSpinnerItem("textareaw", I18N.message("attrtextareaw"),
				Integer.parseInt(Util.getParameterValue(settings, "gui.textarea.w", "250").trim()));
		attrTextAreaW.setHint(PIXELS);
		attrTextAreaW.setRequired(true);
		attrTextAreaW.setWrapTitle(false);
		attrTextAreaW.setStep(50);
		attrTextAreaW.setMin(50);
		
		SpinnerItem attrTextAreaH = ItemFactory.newSpinnerItem("textareah", I18N.message("attrtextareah"),
				Integer.parseInt(Util.getParameterValue(settings, "gui.textarea.h", "100").trim()));
		attrTextAreaH.setHint(PIXELS);
		attrTextAreaH.setRequired(true);
		attrTextAreaH.setWrapTitle(false);
		attrTextAreaH.setStep(50);
		attrTextAreaH.setMin(50);

		SpinnerItem attrTextBoxW = ItemFactory.newSpinnerItem("textboxw", I18N.message("attrtextboxw"),
				Integer.parseInt(Util.getParameterValue(settings, "gui.textbox.w", "150").trim()));
		attrTextBoxW.setHint(PIXELS);
		attrTextBoxW.setRequired(true);
		attrTextBoxW.setWrapTitle(false);
		attrTextBoxW.setStep(50);
		attrTextBoxW.setMin(50);

		SpinnerItem noteMaxSize = ItemFactory.newSpinnerItem("notemaxsize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.note.maxlength", "4000").trim()));
		noteMaxSize.setHint(I18N.message("chars").toLowerCase());
		noteMaxSize.setRequired(true);
		noteMaxSize.setWrapTitle(false);
		noteMaxSize.setStep(100);
		noteMaxSize.setMin(0);
		noteMaxSize.setWidth(70);
		
		SpinnerItem emailMaxSize = ItemFactory.newSpinnerItem("emailmaxsize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.email.maxlength", "20000").trim()));
		emailMaxSize.setHint(I18N.message("chars").toLowerCase());
		emailMaxSize.setRequired(true);
		emailMaxSize.setWrapTitle(false);
		emailMaxSize.setStep(100);
		emailMaxSize.setMin(0);
		emailMaxSize.setWidth(70);

		SpinnerItem thumbSize = ItemFactory.newSpinnerItem("thumbsize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.thumbnail.size", "200").trim()));
		thumbSize.setHint(PIXELS);
		thumbSize.setRequired(true);
		thumbSize.setWrapTitle(false);
		thumbSize.setMin(10);
		thumbSize.setStep(10);

		SpinnerItem thumbQuality = ItemFactory.newSpinnerItem("thumbquality",
				Integer.parseInt(Util.getParameterValue(settings, "gui.thumbnail.quality", "90").trim()));
		thumbQuality.setHint("%");
		thumbQuality.setRequired(true);
		thumbQuality.setWrapTitle(false);
		thumbQuality.setMin(1);
		thumbQuality.setStep(10);
		thumbQuality.setMax(100);

		SpinnerItem mobileSize = ItemFactory.newSpinnerItem("mobilesize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.mobile.size", "336").trim()));
		mobileSize.setHint(PIXELS);
		mobileSize.setRequired(true);
		mobileSize.setWrapTitle(false);
		mobileSize.setMin(10);
		mobileSize.setStep(10);

		SpinnerItem mobileQuality = ItemFactory.newSpinnerItem("mobilequality",
				Integer.parseInt(Util.getParameterValue(settings, "gui.mobile.quality", "90").trim()));
		mobileQuality.setHint("%");
		mobileQuality.setRequired(true);
		mobileQuality.setWrapTitle(false);
		mobileQuality.setMin(1);
		mobileQuality.setStep(10);

		SpinnerItem tileSize = ItemFactory.newSpinnerItem("tilesize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.tile.size", "800").trim()));
		tileSize.setHint(PIXELS);
		tileSize.setRequired(true);
		tileSize.setMin(1);
		tileSize.setStep(10);

		SpinnerItem tileQuality = ItemFactory.newSpinnerItem("tilequality",
				Integer.parseInt(Util.getParameterValue(settings, "gui.tile.quality", "90").trim()));
		tileQuality.setHint("%");
		tileQuality.setRequired(true);
		tileQuality.setWrapTitle(false);
		tileQuality.setMin(1);
		tileQuality.setStep(10);
		tileQuality.setMax(100);
		
		SpinnerItem uploadMax = ItemFactory.newSpinnerItem("uploadmax",
				Integer.parseInt(Util.getParameterValue(settings, "upload.maxsize", "100").trim()));
		uploadMax.setHint("MB");
		uploadMax.setRequired(true);
		uploadMax.setWrapTitle(false);
		uploadMax.setMin(0);
		uploadMax.setStep(10);
		uploadMax.setWidth(70);

		SpinnerItem previewMaxFileSize = ItemFactory.newSpinnerItem("previewmaxfilesize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.preview.maxfilesize", "0").trim()));
		previewMaxFileSize.setHint("MB");
		previewMaxFileSize.setRequired(true);
		previewMaxFileSize.setWrapTitle(false);
		previewMaxFileSize.setMin(0);
		previewMaxFileSize.setStep(1);

		TextItem uploadDisallow = ItemFactory.newTextItem("disallow", "disallowedexts",
				Util.getParameterValue(settings, "upload.disallow"));
		uploadDisallow.setHint(I18N.message("separatedcomma"));
		uploadDisallow.setWidth(350);
		uploadDisallow.setRequired(false);
		uploadDisallow.setWrapTitle(false);

		TextItem textExtensions = ItemFactory.newTextItem("textextensions",
				Util.getParameterValue(settings, "gui.text.extensions"));
		textExtensions.setHint(I18N.message("separatedcomma"));
		textExtensions.setWidth(350);
		textExtensions.setRequired(false);
		textExtensions.setWrapTitle(false);
		
		RadioGroupItem ondoubleclick = ItemFactory.newRadioGroup("ondoubleclick");
		ondoubleclick.setValueMap(DOWNLOAD, "preview");
		ondoubleclick.setValue(Util.getParameterValue(settings, "gui.doubleclick"));

		RadioGroupItem docTab = ItemFactory.newRadioGroup("doctab");
		docTab.setValueMap("properties", "preview");
		docTab.setValue(Util.getParameterValue(settings, "gui.document.tab"));

		RadioGroupItem foldSorting = ItemFactory.newRadioGroup("foldsorting");
		foldSorting.setValueMap("name", "date");
		foldSorting.setValue(Util.getParameterValue(settings, "gui.folder.sorting"));

		RadioGroupItem downloadTicketBehavior = ItemFactory.newRadioGroup("downloadticketbehavior");
		downloadTicketBehavior.setValueMap(DOWNLOAD, "display");
		downloadTicketBehavior.setValue(Util.getParameterValue(settings, "downloadticket.behavior"));

		RadioGroupItem webstartMode = ItemFactory.newRadioGroup("webstartmode");
		webstartMode.setValueMap("webstart", DOWNLOAD);
		webstartMode.setValue(Util.getParameterValue(settings, "gui.webstart.mode"));
		
		ToggleItem foldOpentree = ItemFactory.newToggleItem("foldopentree", "openfolderstree",
				Util.getParameterValueAsBoolean(settings, "gui.folder.opentree"));
		foldOpentree.setWrapTitle(false);

		ToggleItem foldOpenSelect = ItemFactory.newToggleItem("foldopenselect",
				Util.getParameterValueAsBoolean(settings, "gui.folder.openonselect"));
		foldOpenSelect.setWrapTitle(false);

		SpinnerItem maxHistories = ItemFactory.newSpinnerItem("maxhistories",
				Integer.parseInt(Util.getParameterValue(settings, "gui.maxhistories").trim()));
		maxHistories.setStep(10);
		maxHistories.setWrapTitle(false);

		SpinnerItem maxVersions = ItemFactory.newSpinnerItem("maxversions",
				Integer.parseInt(Util.getParameterValue(settings, "gui.maxversions").trim()));
		maxVersions.setStep(10);
		maxVersions.setWrapTitle(false);

		ToggleItem foldPagination = ItemFactory.newToggleItem("foldpagination",
				Util.getParameterValueAsBoolean(settings, "gui.folder.pagination"));
		foldPagination.setWrapTitle(false);

		SpinnerItem foldPageSize = ItemFactory.newSpinnerItem("foldpagesize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.folder.maxchildren").trim()));
		foldPageSize.setWrapTitle(false);

		ToggleItem autocloseFolderNodes = ItemFactory.newToggleItem("autoclosefoldernodes",
				Util.getParameterValueAsBoolean(settings, "gui.folder.autoclose"));

		ToggleItem securityOption = ItemFactory.newToggleItem("securityoption",
				Util.getParameterValueAsBoolean(settings, "gui.security.inheritoption"));
		securityOption.setWrapTitle(false);

		SelectItem securitySecurityOptionDefault = ItemFactory.newFolderSecurityOption("securityoptiondef");
		securitySecurityOptionDefault.setWrapTitle(false);
		securitySecurityOptionDefault
				.setValue(Util.getParameterValue(settings, "gui.security.inheritoption.default", "inherit"));

		TextItem webcontentFolders = ItemFactory.newTextItem("webcontentfolders",
				Util.getParameterValue(settings, "gui.webcontent.folders"));
		webcontentFolders.setHint(I18N.message("commaseplistofids"));
		webcontentFolders.setWidth(350);

		SpinnerItem sessionTimeout = ItemFactory.newSpinnerItem("sessiontimeout",
				Integer.parseInt(Util.getParameterValue(settings, "session.timeout", "30").trim()));
		sessionTimeout.setHint(I18N.message("minutes"));
		sessionTimeout.setRequired(true);
		sessionTimeout.setWrapTitle(false);
		sessionTimeout.setMin(1);
		sessionTimeout.setStep(5);
		
		SpinnerItem rpcTimeout = ItemFactory.newSpinnerItem("rpctimeout",
				Integer.parseInt(Util.getParameterValue(settings, "gui.rpc.timeout", "2").trim()));
		rpcTimeout.setHint(I18N.message("minutes"));
		rpcTimeout.setRequired(true);
		rpcTimeout.setWrapTitle(false);
		rpcTimeout.setMin(1);
		rpcTimeout.setStep(1);

		SpinnerItem sessionHeartbeat = ItemFactory.newSpinnerItem("sessionheartbeat",
				Integer.parseInt(Util.getParameterValue(settings, "session.heartbeat", "60").trim()));
		sessionHeartbeat.setHint(I18N.message(SECONDS));
		sessionHeartbeat.setRequired(true);
		sessionHeartbeat.setWrapTitle(false);
		sessionHeartbeat.setMin(0);
		sessionHeartbeat.setStep(10);

		SpinnerItem popupTimeout = ItemFactory.newSpinnerItem("popuptimeout",
				Integer.parseInt(Util.getParameterValue(settings, "gui.popup.timeout", "4").trim()));
		popupTimeout.setHint(I18N.message(SECONDS));
		popupTimeout.setRequired(true);
		popupTimeout.setWrapTitle(false);
		popupTimeout.setMin(1);
		popupTimeout.setStep(1);

		ToggleItem askVersionCommentOnSave = ItemFactory.newToggleItem("askversioncommentonsave",
				Util.getParameterValueAsBoolean(settings, "gui.onsave.askversioncomment"));
		askVersionCommentOnSave.setWrapTitle(false);
		
		ToggleItem lockOnEditing = ItemFactory.newToggleItem("lockonediting",
				Util.getParameterValueAsBoolean(settings, "gui.onedit.lock"));
		lockOnEditing.setWrapTitle(false);

		SpinnerItem avatarSize = ItemFactory.newSpinnerItem("avatarsize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.avatar.size", "128").trim()));
		avatarSize.setHint(PIXELS);
		avatarSize.setRequired(true);
		avatarSize.setWrapTitle(false);
		avatarSize.setStep(16);
		avatarSize.setMin(16);
		
		SpinnerItem wfDashletRows = ItemFactory.newSpinnerItem("wfdashletrows",
				Integer.parseInt(Util.getParameterValue(settings, "gui.wf.dashlet.rows", "50").trim()));
		wfDashletRows.setRequired(true);
		wfDashletRows.setWrapTitle(false);
		wfDashletRows.setMin(5);
		wfDashletRows.setStep(10);
		
		ToggleItem showAvatarsInGrids = ItemFactory.newToggleItem("showavatarsingrids",
				Util.getParameterValueAsBoolean(settings, "gui.avatar.showingrids"));
		showAvatarsInGrids.setWrapTitle(false);

		SelectItem charset = ItemFactory.newCharsetSelector(CHARSET);
		charset.setValue(Util.getParameterValue(settings, CHARSET));

		ButtonItem save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.addClickHandler((ClickEvent event) -> {
			if (Boolean.FALSE.equals(vm.validate()))
				return;
			onSave();
		});

		parametersForm.setItems(welcome, density, banner, previewSize, previewTimeout, previewMaxFileSize, uploadMax,
				thumbSize, thumbQuality, tileSize, tileQuality, mobileSize, mobileQuality, avatarSize, uploadDisallow,
				showAvatarsInGrids, textExtensions, attrTextBoxW, attrTextAreaW, attrTextAreaH, noteMaxSize,
				emailMaxSize, wfDashletRows, ondoubleclick, docTab, foldSorting, securityOption,
				securitySecurityOptionDefault, foldOpentree, foldOpenSelect, foldPagination, foldPageSize,
				showDocAttrsAsLinks, openPreviewPanel, maxHistories, maxVersions, autocloseFolderNodes, webstartMode,
				galleryEnabled, allowNotesEditing, webcontentFolders, downloadTicketBehavior, saveLogin, sessionTimeout,
				rpcTimeout, sessionHeartbeat, popupTimeout, charset, lockOnEditing, askVersionCommentOnSave,
				reactToRemoteEvents, showPushErrors, saveInputs, showVersionAlertsInLogin, showLicenseAlertsInLogin,
				showQuotaAlertsInLogin, showUpdateAlertsInLogin, showPatchAlertsInLogin, showLanguageInLogin,
				showLostPassword, save);
	}

	private void onSave() {
		List<GUIParameter> params = collectSettings();

		// Update the current session parameters.
		for (GUIParameter p : params)
			Session.get().getInfo().setConfig(p.getName(), p.getValue());

		SettingService.Instance.get().saveSettings(params, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void ret) {
				GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}

	private List<GUIParameter> collectSettings() {

		List<GUIParameter> params = new ArrayList<>();

		collectBooleanSwitches(params);

		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.welcome", vm.getValueAsString("welcome")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.density", vm.getValueAsString("density")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.preview.size",
				vm.getValueAsString("previewsize")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.preview.timeout",
				vm.getValueAsString("previewtimeout")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.rpc.timeout",
				vm.getValueAsString("rpctimeout")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.popup.timeout",
				vm.getValueAsString("popuptimeout")));
		params.add(
				new GUIParameter(Session.get().getTenantName() + ".gui.textarea.w", vm.getValueAsString("textareaw")));
		params.add(
				new GUIParameter(Session.get().getTenantName() + ".gui.textarea.h", vm.getValueAsString("textareah")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.textbox.w", vm.getValueAsString("textboxw")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.thumbnail.size",
				vm.getValueAsString("thumbsize")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.thumbnail.quality",
				vm.getValueAsString("thumbquality")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.mobile.size",
				vm.getValueAsString("mobilesize")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.mobile.quality",
				vm.getValueAsString("mobilequality")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.tile.size", vm.getValueAsString("tilesize")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.tile.quality",
				vm.getValueAsString("tilequality")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.note.maxlength",
				vm.getValueAsString("notemaxsize")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.email.maxlength",
				vm.getValueAsString("emailmaxsize")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.doubleclick",
				vm.getValueAsString("ondoubleclick")));
		params.add(
				new GUIParameter(Session.get().getTenantName() + ".gui.document.tab", vm.getValueAsString("doctab")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.sorting",
				vm.getValueAsString("foldsorting")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.maxchildren",
				vm.getValueAsString("foldpagesize")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.maxhistories",
				vm.getValueAsString("maxhistories")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.maxversions",
				vm.getValueAsString("maxversions")));
		params.add(
				new GUIParameter(Session.get().getTenantName() + ".upload.maxsize", vm.getValueAsString("uploadmax")));
		params.add(
				new GUIParameter(Session.get().getTenantName() + ".upload.disallow", vm.getValueAsString("disallow")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.webcontent.folders",
				vm.getValueAsString("webcontentfolders")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".session.timeout",
				vm.getValueAsString("sessiontimeout")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".session.heartbeat",
				vm.getValueAsString("sessionheartbeat")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".downloadticket.behavior",
				vm.getValueAsString("downloadticketbehavior")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.preview.maxfilesize",
				vm.getValueAsString("previewmaxfilesize")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.webstart.mode",
				vm.getValueAsString("webstartmode")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.avatar.size",
				vm.getValueAsString("avatarsize")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.wf.dashlet.rows",
				vm.getValueAsString("wfdashletrows")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".charset", vm.getValueAsString(CHARSET)));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.security.inheritoption.default",
				vm.getValueAsString("securityoptiondef")));
		return params;
	}

	private void collectBooleanSwitches(List<GUIParameter> params) {
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.banner", vm.getValueAsString("banner")));
		params.add(
				new GUIParameter(Session.get().getTenantName() + ".gui.savelogin", vm.getValueAsString("savelogin")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.preview.openpanel",
				vm.getValueAsString("openpreviewpanel")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.showdocattrsaslinks",
				vm.getValueAsString("showdocattrsaslinks")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.opentree",
				vm.getValueAsString("foldopentree")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.openonselect",
				vm.getValueAsString("foldopenselect")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.pagination",
				vm.getValueAsString("foldpagination")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.autoclose",
				vm.getValueAsString("autoclosefoldernodes")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.serverpush",
				vm.getValueAsString("reacttoremoteevents")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.serverpush.showerror",
				vm.getValueAsString("reacttoremoteeventsshowerrors")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.license.showloginalerts",
				vm.getValueAsString("showlicensealertsinlogin")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.quota.showloginalerts",
				vm.getValueAsString("showquotaalertsinlogin")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.update.showloginalerts",
				vm.getValueAsString("showupdatealertsinlogin")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.version.showloginalerts",
				vm.getValueAsString("showversionalertsinlogin")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.patch.showloginalerts",
				vm.getValueAsString("showpatchalertsinlogin")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.security.inheritoption",
				vm.getValueAsString("securityoption")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.login.lang",
				vm.getValueAsString("showlanguageinlogin")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.lostpassword.show",
				vm.getValueAsString("showlostpassword")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.onsave.askversioncomment",
				vm.getValueAsString("askversioncommentonsave")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.onedit.lock",
				vm.getValueAsString("lockonediting")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.galleryenabled",
				vm.getValueAsString("galleryenabled")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.notes.allowedit",
				vm.getValueAsString("allownotesediting")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.avatar.showingrids",
				vm.getValueAsString("showavatarsingrids")));
		params.add(
				new GUIParameter(Session.get().getTenantName() + ".gui.saveinputs", vm.getValueAsString("saveinputs")));
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}