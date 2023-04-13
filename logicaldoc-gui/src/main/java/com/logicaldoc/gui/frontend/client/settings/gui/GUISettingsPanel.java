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

		TextItem welcome = ItemFactory.newTextItemForAutomation("welcome", I18N.message("welcomemessage"),
				Util.getParameterValue(settings, "gui.welcome"), null);
		welcome.setWidth(400);

		SelectItem density = ItemFactory.newDensitySelector();
		density.setValue(Util.getParameterValue(settings, "gui.density").trim());

		RadioGroupItem saveLogin = ItemFactory.newBooleanSelector("savelogin");
		saveLogin.setHint(I18N.message("saveloginhint"));
		saveLogin.setWrapTitle(false);
		saveLogin.setValue(yesNo(settings, "gui.savelogin"));

		SpinnerItem previewSize = ItemFactory.newSpinnerItem("previewsize", "previewwindow",
				Integer.parseInt(Util.getParameterValue(settings, "gui.preview.size").trim()));
		previewSize.setHint("%");
		previewSize.setRequired(true);
		previewSize.setWrapTitle(false);
		previewSize.setMax(100);
		previewSize.setMin(1);
		previewSize.setStep(10);

		SpinnerItem previewTimeout = ItemFactory.newSpinnerItem("previewtimeout",
				Integer.parseInt(Util.getParameterValue(settings, "gui.preview.timeout").trim()));
		previewTimeout.setHint(I18N.message(SECONDS));
		previewTimeout.setWrapTitle(false);
		previewTimeout.setRequired(true);
		previewSize.setMin(1);
		previewSize.setStep(10);

		RadioGroupItem banner = ItemFactory.newBooleanSelector("banner");
		banner.setWrapTitle(false);
		banner.setValue(yesNo(settings, "gui.banner"));

		RadioGroupItem openPreviewPanel = ItemFactory.newBooleanSelector("openpreviewpanel");
		openPreviewPanel.setWrapTitle(false);
		openPreviewPanel.setValue(yesNo(settings, "gui.preview.openpanel"));

		RadioGroupItem reactToRemoteEvents = ItemFactory.newBooleanSelector("reacttoremoteevents");
		reactToRemoteEvents.setWrapTitle(false);
		reactToRemoteEvents.setValue(yesNo(settings, "gui.serverpush"));

		RadioGroupItem showLicenseAlertsInLogin = ItemFactory.newBooleanSelector("showlicensealertsinlogin");
		showLicenseAlertsInLogin.setWrapTitle(false);
		showLicenseAlertsInLogin.setValue(yesNo(settings, "gui.license.showloginalerts"));

		RadioGroupItem showQuotaAlertsInLogin = ItemFactory.newBooleanSelector("showquotaalertsinlogin");
		showQuotaAlertsInLogin.setWrapTitle(false);
		showQuotaAlertsInLogin.setValue(yesNo(settings, "gui.quota.showloginalerts"));

		RadioGroupItem showUpdateAlertsInLogin = ItemFactory.newBooleanSelector("showupdatealertsinlogin");
		showUpdateAlertsInLogin.setWrapTitle(false);
		showUpdateAlertsInLogin.setValue(yesNo(settings, "gui.update.showloginalerts"));

		RadioGroupItem showVersionAlertsInLogin = ItemFactory.newBooleanSelector("showversionalertsinlogin");
		showVersionAlertsInLogin.setWrapTitle(false);
		showVersionAlertsInLogin.setValue(yesNo(settings, "gui.version.showloginalerts"));

		RadioGroupItem showPatchAlertsInLogin = ItemFactory.newBooleanSelector("showpatchalertsinlogin");
		showPatchAlertsInLogin.setWrapTitle(false);
		showPatchAlertsInLogin.setValue(yesNo(settings, "gui.patch.showloginalerts"));

		RadioGroupItem showLanguageInLogin = ItemFactory.newBooleanSelector("showlanguageinlogin");
		showLicenseAlertsInLogin.setWrapTitle(false);
		showLanguageInLogin.setValue(yesNo(settings, "gui.login.lang"));

		RadioGroupItem showLostPassword = ItemFactory.newBooleanSelector("showlostpassword",
				I18N.message("showlostpasswordlink"));
		showLostPassword.setWrapTitle(false);
		showLostPassword.setValue(yesNo(settings, "gui.lostpassword.show"));

		RadioGroupItem galleryEnabled = ItemFactory.newBooleanSelector("galleryenabled");
		galleryEnabled.setWrapTitle(false);
		galleryEnabled.setValue(yesNo(settings, "gui.galleryenabled"));

		RadioGroupItem allowNotesEditing = ItemFactory.newBooleanSelector("allownotesediting");
		allowNotesEditing.setWrapTitle(false);
		allowNotesEditing.setValue(yesNo(settings, "gui.notes.allowedit"));

		RadioGroupItem saveInputs = ItemFactory.newBooleanSelector("saveinputs");
		galleryEnabled.setWrapTitle(false);
		saveInputs.setValue(yesNo(settings, "gui.saveinputs"));

		SpinnerItem attrTextAreaW = ItemFactory.newSpinnerItem("textareaw", I18N.message("attrtextareaw"),
				Integer.parseInt(Util.getParameterValue(settings, "gui.textarea.w").trim()));
		attrTextAreaW.setHint(PIXELS);
		attrTextAreaW.setRequired(true);
		attrTextAreaW.setWrapTitle(false);
		attrTextAreaW.setStep(50);
		attrTextAreaW.setMin(50);

		SpinnerItem attrTextAreaH = ItemFactory.newSpinnerItem("textareah", I18N.message("attrtextareah"),
				Integer.parseInt(Util.getParameterValue(settings, "gui.textarea.h").trim()));
		attrTextAreaH.setHint(PIXELS);
		attrTextAreaH.setRequired(true);
		attrTextAreaH.setWrapTitle(false);
		attrTextAreaH.setStep(50);
		attrTextAreaH.setMin(50);

		SpinnerItem attrTextBoxW = ItemFactory.newSpinnerItem("textboxw", I18N.message("attrtextboxw"),
				Integer.parseInt(Util.getParameterValue(settings, "gui.textbox.w").trim()));
		attrTextBoxW.setHint(PIXELS);
		attrTextBoxW.setRequired(true);
		attrTextBoxW.setWrapTitle(false);
		attrTextBoxW.setStep(50);
		attrTextBoxW.setMin(50);

		SpinnerItem noteMaxSize = ItemFactory.newSpinnerItem("notemaxsize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.note.maxlength").trim()));
		noteMaxSize.setHint(I18N.message("chars").toLowerCase());
		noteMaxSize.setRequired(true);
		noteMaxSize.setWrapTitle(false);
		noteMaxSize.setStep(100);
		noteMaxSize.setMin(0);
		noteMaxSize.setWidth(70);

		SpinnerItem emailMaxSize = ItemFactory.newSpinnerItem("emailmaxsize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.email.maxlength").trim()));
		emailMaxSize.setHint(I18N.message("chars").toLowerCase());
		emailMaxSize.setRequired(true);
		emailMaxSize.setWrapTitle(false);
		emailMaxSize.setStep(100);
		emailMaxSize.setMin(0);
		emailMaxSize.setWidth(70);

		SpinnerItem thumbSize = ItemFactory.newSpinnerItem("thumbsize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.thumbnail.size").trim()));
		thumbSize.setHint(PIXELS);
		thumbSize.setRequired(true);
		thumbSize.setWrapTitle(false);
		thumbSize.setMin(10);
		thumbSize.setStep(10);

		SpinnerItem thumbQuality = ItemFactory.newSpinnerItem("thumbquality",
				Integer.parseInt(Util.getParameterValue(settings, "gui.thumbnail.quality").trim()));
		thumbQuality.setHint("%");
		thumbQuality.setRequired(true);
		thumbQuality.setWrapTitle(false);
		thumbQuality.setMin(1);
		thumbQuality.setStep(10);
		thumbQuality.setMax(100);

		SpinnerItem mobileSize = ItemFactory.newSpinnerItem("mobilesize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.mobile.size").trim()));
		mobileSize.setHint(PIXELS);
		mobileSize.setRequired(true);
		mobileSize.setWrapTitle(false);
		mobileSize.setMin(10);
		mobileSize.setStep(10);

		SpinnerItem mobileQuality = ItemFactory.newSpinnerItem("mobilequality",
				Integer.parseInt(Util.getParameterValue(settings, "gui.mobile.quality").trim()));
		mobileQuality.setHint("%");
		mobileQuality.setRequired(true);
		mobileQuality.setWrapTitle(false);
		mobileQuality.setMin(1);
		mobileQuality.setStep(10);

		SpinnerItem tileSize = ItemFactory.newSpinnerItem("tilesize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.tile.size").trim()));
		tileSize.setHint(PIXELS);
		tileSize.setRequired(true);
		tileSize.setMin(1);
		tileSize.setStep(10);

		SpinnerItem tileQuality = ItemFactory.newSpinnerItem("tilequality",
				Integer.parseInt(Util.getParameterValue(settings, "gui.tile.quality").trim()));
		tileQuality.setHint("%");
		tileQuality.setRequired(true);
		tileQuality.setWrapTitle(false);
		tileQuality.setMin(1);
		tileQuality.setStep(10);
		tileQuality.setMax(100);

		SpinnerItem uploadMax = ItemFactory.newSpinnerItem("uploadmax",
				Integer.parseInt(Util.getParameterValue(settings, "upload.maxsize").trim()));
		uploadMax.setHint("MB");
		uploadMax.setRequired(true);
		uploadMax.setWrapTitle(false);
		uploadMax.setMin(0);
		uploadMax.setStep(10);
		uploadMax.setWidth(70);

		SpinnerItem previewMaxFileSize = ItemFactory.newSpinnerItem("previewmaxfilesize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.preview.maxfilesize").trim()));
		previewMaxFileSize.setHint("MB");
		previewMaxFileSize.setRequired(true);
		previewMaxFileSize.setWrapTitle(false);
		previewMaxFileSize.setMin(0);
		previewMaxFileSize.setStep(1);

		TextItem uploadDisallow = ItemFactory.newTextItem("disallow", "disallowedext",
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

		RadioGroupItem ondoubleclick = ItemFactory.newBooleanSelector("ondoubleclick");
		ondoubleclick.setValueMap(DOWNLOAD, "preview");
		ondoubleclick.setValue(Util.getParameterValue(settings, "gui.doubleclick"));

		RadioGroupItem docTab = ItemFactory.newBooleanSelector("doctab");
		docTab.setValueMap("properties", "preview");
		docTab.setValue(Util.getParameterValue(settings, "gui.document.tab"));

		RadioGroupItem foldSorting = ItemFactory.newBooleanSelector("foldsorting");
		foldSorting.setValueMap("name", "date");
		foldSorting.setValue(Util.getParameterValue(settings, "gui.folder.sorting"));

		RadioGroupItem downloadTicketBehavior = ItemFactory.newBooleanSelector("downloadticketbehavior");
		downloadTicketBehavior.setValueMap(DOWNLOAD, "display");
		downloadTicketBehavior.setValue(Util.getParameterValue(settings, "downloadticket.behavior"));

		RadioGroupItem webstartMode = ItemFactory.newBooleanSelector("webstartmode");
		webstartMode.setValueMap("webstart", DOWNLOAD);
		webstartMode.setValue(Util.getParameterValue(settings, "gui.webstart.mode"));

		RadioGroupItem foldOpentree = ItemFactory.newBooleanSelector("foldopentree","openfolderstree");
		foldOpentree.setWrapTitle(false);
		foldOpentree.setValue(yesNo(settings, "gui.folder.opentree"));

		RadioGroupItem foldOpenSelect = ItemFactory.newBooleanSelector("foldopenselect");
		foldOpenSelect.setWrapTitle(false);
		foldOpenSelect.setValue(yesNo(settings, "gui.folder.openonselect"));

		SpinnerItem foldMaxChildren = ItemFactory.newSpinnerItem("foldmaxchildren",
				Integer.parseInt(Util.getParameterValue(settings, "gui.folder.maxchildren").trim()));
		foldMaxChildren.setWrapTitle(false);

		SpinnerItem maxHistories = ItemFactory.newSpinnerItem("maxhistories",
				Integer.parseInt(Util.getParameterValue(settings, "gui.maxhistories").trim()));
		maxHistories.setStep(10);
		maxHistories.setWrapTitle(false);

		RadioGroupItem foldPagination = ItemFactory.newBooleanSelector("foldpagination");
		foldPagination.setWrapTitle(false);
		foldPagination.setValue(yesNo(settings, "gui.folder.pagination"));

		RadioGroupItem autocloseFolderNodes = ItemFactory.newBooleanSelector("autoclosefoldernodes");
		autocloseFolderNodes.setValue(yesNo(settings, "gui.folder.autoclose"));

		RadioGroupItem securityOption = ItemFactory.newBooleanSelector("securityoption");
		securityOption.setWrapTitle(false);
		securityOption.setValue(yesNo(settings, "gui.security.inheritoption"));

		SelectItem securitySecurityOptionDefault = ItemFactory.newFolderSecurityOption("securityoptiondef");
		securitySecurityOptionDefault.setWrapTitle(false);
		securitySecurityOptionDefault.setValue(Util.getParameterValue(settings, "gui.security.inheritoption.default"));

		TextItem webcontentFolders = ItemFactory.newTextItem("webcontentfolders",
				Util.getParameterValue(settings, "gui.webcontent.folders"));
		webcontentFolders.setHint(I18N.message("commaseplistofids"));
		webcontentFolders.setWidth(350);

		SpinnerItem sessionTimeout = ItemFactory.newSpinnerItem("sessiontimeout",
				Integer.parseInt(Util.getParameterValue(settings, "session.timeout").trim()));
		sessionTimeout.setHint(I18N.message("minutes"));
		sessionTimeout.setRequired(true);
		sessionTimeout.setWrapTitle(false);
		sessionTimeout.setMin(1);
		sessionTimeout.setStep(5);

		SpinnerItem rpcTimeout = ItemFactory.newSpinnerItem("rpctimeout",
				Integer.parseInt(Util.getParameterValue(settings, "gui.rpc.timeout").trim()));
		rpcTimeout.setHint(I18N.message("minutes"));
		rpcTimeout.setRequired(true);
		rpcTimeout.setWrapTitle(false);
		rpcTimeout.setMin(1);
		rpcTimeout.setStep(1);

		SpinnerItem sessionHeartbeat = ItemFactory.newSpinnerItem("sessionheartbeat",
				Integer.parseInt(Util.getParameterValue(settings, "session.heartbeat").trim()));
		sessionHeartbeat.setHint(I18N.message(SECONDS));
		sessionHeartbeat.setRequired(true);
		sessionHeartbeat.setWrapTitle(false);
		sessionHeartbeat.setMin(0);
		sessionHeartbeat.setStep(10);

		SpinnerItem popupTimeout = ItemFactory.newSpinnerItem("popuptimeout",
				Integer.parseInt(Util.getParameterValue(settings, "gui.popup.timeout").trim()));
		popupTimeout.setHint(I18N.message(SECONDS));
		popupTimeout.setRequired(true);
		popupTimeout.setWrapTitle(false);
		popupTimeout.setMin(1);
		popupTimeout.setStep(1);

		RadioGroupItem askVersionCommentOnSave = ItemFactory.newBooleanSelector("askversioncommentonsave");
		askVersionCommentOnSave.setWrapTitle(false);
		askVersionCommentOnSave.setValue(yesNo(settings, "gui.onsave.askversioncomment"));

		RadioGroupItem lockOnEditing = ItemFactory.newBooleanSelector("lockonediting");
		lockOnEditing.setWrapTitle(false);
		lockOnEditing.setValue(yesNo(settings, "gui.onedit.lock"));

		SpinnerItem avatarSize = ItemFactory.newSpinnerItem("avatarsize",
				Integer.parseInt(Util.getParameterValue(settings, "gui.avatar.size").trim()));
		avatarSize.setHint(PIXELS);
		avatarSize.setRequired(true);
		avatarSize.setWrapTitle(false);
		avatarSize.setStep(16);
		avatarSize.setMin(16);

		SpinnerItem wfDashletRows = ItemFactory.newSpinnerItem("wfdashletrows",
				Integer.parseInt(Util.getParameterValue(settings, "gui.wf.dashlet.rows").trim()));
		wfDashletRows.setRequired(true);
		wfDashletRows.setWrapTitle(false);
		wfDashletRows.setMin(5);
		wfDashletRows.setStep(10);

		RadioGroupItem showAvatarsInGrids = ItemFactory.newBooleanSelector("showavatarsingrids");
		showAvatarsInGrids.setWrapTitle(false);
		showAvatarsInGrids.setValue(yesNo(settings, "gui.avatar.showingrids"));

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
				securitySecurityOptionDefault, foldOpentree, foldOpenSelect, foldPagination, foldMaxChildren,
				openPreviewPanel, maxHistories, autocloseFolderNodes, webstartMode, galleryEnabled, allowNotesEditing,
				webcontentFolders, downloadTicketBehavior, saveLogin, sessionTimeout, rpcTimeout, sessionHeartbeat,
				popupTimeout, charset, lockOnEditing, askVersionCommentOnSave, reactToRemoteEvents, saveInputs,
				showVersionAlertsInLogin, showLicenseAlertsInLogin, showQuotaAlertsInLogin, showUpdateAlertsInLogin,
				showPatchAlertsInLogin, showLanguageInLogin, showLostPassword, save);
	}

	private void onSave() {
		List<GUIParameter> params = collectSettings();

		// Update the current session parameters.
		for (GUIParameter p : params)
			Session.get().getInfo().setConfig(p.getName(), p.getValue());

		SettingService.Instance.get().saveSettings(params.toArray(new GUIParameter[0]), new AsyncCallback<Void>() {

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

	private List<GUIParameter> collectSettings() {

		List<GUIParameter> params = new ArrayList<>();

		collectBooleanSwitches(params);

		@SuppressWarnings("unchecked")
		Map<String, Object> values = vm.getValues();
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.welcome", (String) values.get("welcome")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.density", (String) values.get("density")));
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
		params.add(
				new GUIParameter(Session.get().getTenantName() + ".gui.textbox.w", values.get("textboxw").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.thumbnail.size",
				values.get("thumbsize").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.thumbnail.quality",
				values.get("thumbquality").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.mobile.size",
				values.get("mobilesize").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.mobile.quality",
				values.get("mobilequality").toString()));
		params.add(
				new GUIParameter(Session.get().getTenantName() + ".gui.tile.size", values.get("tilesize").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.tile.quality",
				values.get("tilequality").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.note.maxlength",
				values.get("notemaxsize").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.email.maxlength",
				values.get("emailmaxsize").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.doubleclick",
				values.get("ondoubleclick").toString()));
		params.add(
				new GUIParameter(Session.get().getTenantName() + ".gui.document.tab", values.get("doctab").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.sorting",
				values.get("foldsorting").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.maxchildren",
				values.get("foldmaxchildren").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.maxhistories",
				values.get("maxhistories").toString()));
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
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.preview.maxfilesize",
				values.get("previewmaxfilesize").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.webstart.mode",
				values.get("webstartmode").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.avatar.size",
				values.get("avatarsize").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.wf.dashlet.rows",
				values.get("wfdashletrows").toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".charset", values.get(CHARSET).toString()));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.security.inheritoption.default",
				values.get("securityoptiondef").toString()));
		return params;
	}

	private void collectBooleanSwitches(List<GUIParameter> params) {
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.banner", trueFalse("banner")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.savelogin", trueFalse("savelogin")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.preview.openpanel",
				trueFalse("openpreviewpanel")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.opentree", trueFalse("foldopentree")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.openonselect",
				trueFalse("foldopenselect")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.pagination",
				trueFalse("foldpagination")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.folder.autoclose",
				trueFalse("autoclosefoldernodes")));
		params.add(
				new GUIParameter(Session.get().getTenantName() + ".gui.serverpush", trueFalse("reacttoremoteevents")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.license.showloginalerts",
				trueFalse("showlicensealertsinlogin")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.quota.showloginalerts",
				trueFalse("showquotaalertsinlogin")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.update.showloginalerts",
				trueFalse("showupdatealertsinlogin")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.version.showloginalerts",
				trueFalse("showversionalertsinlogin")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.patch.showloginalerts",
				trueFalse("showpatchalertsinlogin")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.security.inheritoption",
				trueFalse("securityoption")));
		params.add(
				new GUIParameter(Session.get().getTenantName() + ".gui.login.lang", trueFalse("showlanguageinlogin")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.lostpassword.show",
				trueFalse("showlostpassword")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.onsave.askversioncomment",
				trueFalse("askversioncommentonsave")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.onedit.lock", trueFalse("lockonediting")));
		params.add(
				new GUIParameter(Session.get().getTenantName() + ".gui.galleryenabled", trueFalse("galleryenabled")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.notes.allowedit",
				trueFalse("allownotesediting")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.avatar.showingrids",
				trueFalse("showavatarsingrids")));
		params.add(new GUIParameter(Session.get().getTenantName() + ".gui.saveinputs", trueFalse("saveinputs")));
	}

	private String yesNo(GUIParameter[] settings, String name) {
		String parameterValue = Util.getParameterValue(settings, name);
		return parameterValue != null && parameterValue.equals("true") ? "yes" : "no";
	}

	private String trueFalse(String name) {
		return "yes".equals(vm.getValues().get(name)) ? "true" : "false";
	}
}