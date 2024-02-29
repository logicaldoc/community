package com.logicaldoc.gui.frontend.client.security.saml;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.CopyTextFormItemIcon;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.LinkItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel shows the Saml settings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class SamlPanel extends VLayout {

	private static final String LOGOUTRESPONSE_SIGNED = "logoutresponsesigned";

	private static final String LOGOUTREQUEST_SIGNED = "logoutrequestsigned";

	private static final String SLO_ENABLED = "sloenabled";

	private static final String SIGNATURE_ALGORITHM = "signaturealgorithm";

	private static final String GROUPS = "groups";

	private static final String EMAIL = "email";

	private static final String LASTNAME = "lastname";

	private static final String FIRSTNAME = "firstname";

	private static final String USERNAME = "username";

	private static final int TEXTAREA_HEIGHT = 120;

	private static final int TEXTAREA_WIDTH = 530;

	private static final String ENABLED = "enabled";

	private static final String SP_ENTITYID = "spentityid";

	private static final String SP_CERTIFICATE = "certificate.cer";

	private static final String SP_PRIVATEKEY = "privatekey.txt";

	private static final String IDP_METADATA = "idpmetadata.xml";

	private static final String AUTHNREQUEST_SIGNED = "authnrequestsigned";

	private static final String ASSERTIONS_ENCRYPTED = "assertionsencrypted";

	private static final String NAMEID_ENCRYPTED = "nameidencrypted";

	private static final String KEEPMEMBERSHIP = "keepmembership";

	public SamlPanel() {
		setWidth100();
		setMembersMargin(5);
		setMargin(5);
	}

	@Override
	protected void onDraw() {
		SamlService.Instance.get().loadSettings(new AsyncCallback<>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUISamlSettings settings) {
				initGUI(settings);
			}
		});
	}

	private void initGUI(GUISamlSettings settings) {
		ValuesManager vm = new ValuesManager();

		RadioGroupItem enabled = ItemFactory.newBooleanSelector(ENABLED);
		enabled.setWrapTitle(false);
		enabled.setRequired(true);
		enabled.setValue(settings.isEnabled() ? "yes" : "no");

		RadioGroupItem sloEnabled = ItemFactory.newBooleanSelector(SLO_ENABLED);
		sloEnabled.setWrapTitle(false);
		sloEnabled.setRequired(true);
		sloEnabled.setRedrawOnChange(true);
		sloEnabled.setValue(settings.isSingleLogOut() ? "yes" : "no");
		sloEnabled.setVisible(false);
		
		RadioGroupItem logoutRequestSigned = ItemFactory.newBooleanSelector(LOGOUTREQUEST_SIGNED);
		logoutRequestSigned.setWrapTitle(true);
		logoutRequestSigned.setRequired(true);
		logoutRequestSigned.setRedrawOnChange(true);
		logoutRequestSigned.setTitleOrientation(TitleOrientation.TOP);
		logoutRequestSigned.setValue(settings.isLogoutRequestSigned() ? "yes" : "no");
		logoutRequestSigned.setVisible(settings.isSingleLogOut());
		logoutRequestSigned.setShowIfCondition((item, value, form) -> "yes".equals(form.getValueAsString(SLO_ENABLED)));

		RadioGroupItem logoutResponseSigned = ItemFactory.newBooleanSelector(LOGOUTRESPONSE_SIGNED);
		logoutResponseSigned.setWrapTitle(true);
		logoutResponseSigned.setRequired(true);
		logoutResponseSigned.setRedrawOnChange(true);
		logoutResponseSigned.setVisible(settings.isSingleLogOut());
		logoutResponseSigned.setTitleOrientation(TitleOrientation.TOP);
		logoutResponseSigned.setValue(settings.isLogoutResponseSigned() ? "yes" : "no");
		logoutResponseSigned
				.setShowIfCondition((item, value, form) -> "yes".equals(form.getValueAsString(SLO_ENABLED)));

		TextItem id = ItemFactory.newTextItem(SP_ENTITYID, SP_ENTITYID, settings.getEntityId());
		id.setWidth(220);
		id.setWrapTitle(false);
		id.setRequired(true);

		TextItem username = ItemFactory.newTextItem(USERNAME, USERNAME, settings.getUsername());
		username.setWrapTitle(false);

		TextItem firstName = ItemFactory.newTextItem(FIRSTNAME, FIRSTNAME, settings.getFirstName());
		firstName.setWrapTitle(false);

		TextItem lastName = ItemFactory.newTextItem(LASTNAME, LASTNAME, settings.getLastName());
		lastName.setWrapTitle(false);

		TextItem email = ItemFactory.newTextItem(EMAIL, EMAIL, settings.getEmail());
		email.setWrapTitle(false);

		TextItem groups = ItemFactory.newTextItem(GROUPS, GROUPS, settings.getGroup());
		groups.setWrapTitle(false);

		RadioGroupItem keepMembership = ItemFactory.newBooleanSelector(KEEPMEMBERSHIP, "keepmembershiplocalgroups");
		keepMembership.setValue(settings.isKeepLocalMemberships() ? "yes" : "no");
		keepMembership.setRequired(true);

		RadioGroupItem authnRequestSigned = ItemFactory.newBooleanSelector(AUTHNREQUEST_SIGNED);
		authnRequestSigned.setWrapTitle(true);
		authnRequestSigned.setRequired(true);
		authnRequestSigned.setRedrawOnChange(true);
		authnRequestSigned.setTitleOrientation(TitleOrientation.TOP);
		authnRequestSigned.setValue(settings.isAuthnRequestSigned() ? "yes" : "no");

		SelectItem signatureAlgorithm = ItemFactory.newSelectItem(SIGNATURE_ALGORITHM);
		signatureAlgorithm.setValueMap("SHA-1", "SHA-256");
		signatureAlgorithm.setValue(settings.getSignatureAlgorithm());
		signatureAlgorithm
				.setShowIfCondition((item, value, form) -> "yes".equals(form.getValueAsString(AUTHNREQUEST_SIGNED)));

		RadioGroupItem assertionsEncrypted = ItemFactory.newBooleanSelector(ASSERTIONS_ENCRYPTED);
		assertionsEncrypted.setWrapTitle(true);
		assertionsEncrypted.setRequired(true);
		assertionsEncrypted.setRedrawOnChange(true);
		assertionsEncrypted.setTitleOrientation(TitleOrientation.TOP);
		assertionsEncrypted.setValue(settings.isWantAssertionsEncrypted() ? "yes" : "no");

		RadioGroupItem nameIdEncrypted = ItemFactory.newBooleanSelector(NAMEID_ENCRYPTED);
		nameIdEncrypted.setWrapTitle(true);
		nameIdEncrypted.setRequired(true);
		nameIdEncrypted.setRedrawOnChange(true);
		nameIdEncrypted.setTitleOrientation(TitleOrientation.TOP);
		nameIdEncrypted.setValue(settings.isWantNameIdEncrypted() ? "yes" : "no");

		TextAreaItem spCertificate = ItemFactory.newTextAreaItem(SP_CERTIFICATE, "spcertificate",
				settings.getCertificate());
		spCertificate.setWrapTitle(false);
		spCertificate.setColSpan(2);
		spCertificate.setWidth(TEXTAREA_WIDTH);
		spCertificate.setHeight(TEXTAREA_HEIGHT);
		spCertificate.setIcons(new CopyTextFormItemIcon(),
				new DownloadFormItemIcon(Util.contextPath() + "saml/spcertificate"),
				new UploadFormItemIcon("uploadspcertificate"));
		spCertificate
				.setShowIfCondition((item, value, form) -> "yes".equals(form.getValueAsString(ASSERTIONS_ENCRYPTED))
						|| "yes".equals(form.getValueAsString(NAMEID_ENCRYPTED))
						|| "yes".equals(form.getValueAsString(AUTHNREQUEST_SIGNED))
						|| "yes".equals(form.getValueAsString(LOGOUTREQUEST_SIGNED))
						|| "yes".equals(form.getValueAsString(LOGOUTRESPONSE_SIGNED)));

		TextAreaItem spPrivateKey = ItemFactory.newTextAreaItem(SP_PRIVATEKEY, "spprivatekey",
				settings.getPrivateKey());
		spPrivateKey.setWrapTitle(false);
		spPrivateKey.setColSpan(2);
		spPrivateKey.setWidth(TEXTAREA_WIDTH);
		spPrivateKey.setHeight(TEXTAREA_HEIGHT);
		spPrivateKey.setIcons(new CopyTextFormItemIcon(),
				new DownloadFormItemIcon(Util.contextPath() + "saml/spprivatekey"),
				new UploadFormItemIcon("uploadspprivatekey"));
		spPrivateKey.setShowIfCondition((item, value, form) -> "yes".equals(form.getValueAsString(ASSERTIONS_ENCRYPTED))
				|| "yes".equals(form.getValueAsString(NAMEID_ENCRYPTED))
				|| "yes".equals(form.getValueAsString(AUTHNREQUEST_SIGNED))
				|| "yes".equals(form.getValueAsString(LOGOUTREQUEST_SIGNED))
				|| "yes".equals(form.getValueAsString(LOGOUTRESPONSE_SIGNED)));

		TextAreaItem idpMetadata = ItemFactory.newTextAreaItem(IDP_METADATA, "idpmetadata", settings.getIdpMetadata());
		idpMetadata.setWrapTitle(false);
		idpMetadata.setColSpan(2);
		idpMetadata.setWidth(TEXTAREA_WIDTH);
		idpMetadata.setHeight(TEXTAREA_HEIGHT);
		idpMetadata.setIcons(new CopyTextFormItemIcon(),
				new DownloadFormItemIcon(Util.contextPath() + "saml/idpmetadata"),
				new UploadFormItemIcon("uploadidpmetadata"));

		String metadataUrl = Util.contextPath() + "saml/spmetadata";
		LinkItem spMetadata = ItemFactory.newLinkItem("spmetadata", "spmetadata", metadataUrl, metadataUrl);
		spMetadata.setWrapTitle(false);
		spMetadata.setWrap(false);

		String loginUrl = Util.contextPath() + "saml/login";
		LinkItem login = ItemFactory.newLinkItem("login", "login", loginUrl, loginUrl);
		login.setWrapTitle(false);
		login.setWrap(false);

		DynamicForm generalForm = new DynamicForm();
		generalForm.setValuesManager(vm);
		generalForm.setTitleOrientation(TitleOrientation.TOP);
		generalForm.setAlign(Alignment.LEFT);
		generalForm.setGroupTitle(I18N.message("general"));
		generalForm.setIsGroup(true);
		generalForm.setHeight(1);
		generalForm.setWidth(1);
		generalForm.setNumCols(2);
		generalForm.setFields(enabled, login, id, authnRequestSigned, signatureAlgorithm, assertionsEncrypted,
				nameIdEncrypted, spMetadata, sloEnabled, logoutRequestSigned, logoutResponseSigned, spCertificate,
				spPrivateKey, idpMetadata);

		DynamicForm attributeMappingsForm = new DynamicForm();
		attributeMappingsForm.setValuesManager(vm);
		attributeMappingsForm.setIsGroup(true);
		attributeMappingsForm.setGroupTitle(I18N.message("attrtibutemappings"));
		attributeMappingsForm.setTitleOrientation(TitleOrientation.TOP);
		attributeMappingsForm.setAlign(Alignment.LEFT);
		attributeMappingsForm.setHeight(1);
		attributeMappingsForm.setWidth(1);
		attributeMappingsForm.setFields(username, firstName, lastName, email, groups, keepMembership);

		HLayout forms = new HLayout();
		forms.setMembersMargin(10);
		forms.setMembers(generalForm, attributeMappingsForm);

		Tab tab = new Tab();
		tab.setTitle(I18N.message("singlesignonsaml"));
		tab.setPane(forms);

		TabSet tabs = new TabSet();
		tabs.setWidth100();
		tabs.setHeight100();
		tabs.setTabs(tab);

		IButton save = prepareSaveButton(vm);
		setMembers(tabs, save);
	}

	private IButton prepareSaveButton(ValuesManager form) {
		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(event -> {
			if (!Boolean.TRUE.equals(form.validate()))
				return;

			GUISamlSettings settings = new GUISamlSettings();
			settings.setEnabled("yes".equals(form.getValue(ENABLED)));
			settings.setEntityId(form.getValueAsString(SP_ENTITYID));
			settings.setUsername(form.getValueAsString(USERNAME));
			settings.setFirstName(form.getValueAsString(FIRSTNAME));
			settings.setLastName(form.getValueAsString(LASTNAME));
			settings.setEmail(form.getValueAsString(EMAIL));
			settings.setGroup(form.getValueAsString(GROUPS));
			settings.setAuthnRequestSigned("yes".equals(form.getValue(AUTHNREQUEST_SIGNED)));
			settings.setSignatureAlgorithm(form.getValueAsString(SIGNATURE_ALGORITHM));
			settings.setWantAssertionsEncrypted("yes".equals(form.getValue(ASSERTIONS_ENCRYPTED)));
			settings.setWantNameIdEncrypted("yes".equals(form.getValue(NAMEID_ENCRYPTED)));
			settings.setKeepLocalMemberships("yes".equals(form.getValue(KEEPMEMBERSHIP)));
			settings.setSingleLogOut("yes".equals(form.getValue(SLO_ENABLED)));
			settings.setLogoutRequestSigned("yes".equals(form.getValue(LOGOUTREQUEST_SIGNED)));
			settings.setLogoutResponseSigned("yes".equals(form.getValue(LOGOUTRESPONSE_SIGNED)));
			settings.setCertificate(form.getValueAsString(SP_CERTIFICATE));
			settings.setPrivateKey(form.getValueAsString(SP_PRIVATEKEY));
			settings.setIdpMetadata(form.getValueAsString(IDP_METADATA));

			SamlService.Instance.get().saveSettings(settings, new AsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void ret) {
					GuiLog.info(I18N.message("settingssaved"), null);
				}
			});
		});
		return save;
	}
}