package com.logicaldoc.gui.frontend.client.tenant;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.EmptyAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIKeystore;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.validators.MinLengthValidator;
import com.logicaldoc.gui.frontend.client.services.SignService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.form.validator.MatchesFieldValidator;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows the tenant's keystore informations.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.2
 */
public class TenantKeystorePanel extends VLayout {

	private static final String PASSWORD = "password";

	private DynamicForm form = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private long tenantId;

	private VLayout layout = null;

	private GUIKeystore keystore = null;

	public TenantKeystorePanel(final long tenantId) {
		this.tenantId = tenantId;
		initGUI();
	}

	void initGUI() {
		if (tenantId <= 0) {
			setMembers(TenantsPanel.SELECT_TENANT);
		} else {
			setWidth100();
			setHeight100();
			setMembersMargin(20);

			SignService.Instance.get().loadKeystore(tenantId, new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(GUIKeystore keystore) {
					TenantKeystorePanel.this.keystore = keystore;
					if (keystore == null) {
						TenantKeystorePanel.this.keystore = new GUIKeystore();
						TenantKeystorePanel.this.keystore.setTenantId(tenantId);
					}
					refresh();
				}
			});
		}
	}

	public TenantKeystorePanel(GUITenant tenant) {
		this(tenant.getId());
	}

	public void refresh() {
		boolean ksAlreadyGenerated = prepareForm();

		TextItem localCAalias = prepareLocalCAaliasItem();

		TextItem password = ItemFactory.newPasswordItemPreventAutocomplete(PASSWORD, "keystorepasswd", null);
		password.setRequired(true);
		password.setWrapTitle(false);
		password.setValidators(new MinLengthValidator(6));

		PasswordItem passwordAgain = ItemFactory.newPasswordItemPreventAutocomplete("passwordagain",
				"keystorepasswdagain", null);
		passwordAgain.setRequired(true);
		passwordAgain.setWrapTitle(false);
		MatchesFieldValidator validator = new MatchesFieldValidator();
		validator.setOtherField(PASSWORD);
		validator.setErrorMessage(I18N.message("passwordnotmatch"));
		passwordAgain.setValidators(validator);

		StaticTextItem details = prepareDetailsItem();

		SpinnerItem validity = prepareValiditySpinner();

		TextItem countryCode = ItemFactory.newTextItem("countrycode", null);
		countryCode.setLength(2);
		countryCode.setHint("C");
		countryCode.setRequired(true);
		countryCode.setWrapTitle(false);

		TextItem organization = ItemFactory.newTextItem("organization", null);
		organization.setHint("O");
		organization.setRequired(true);
		organization.setWrapTitle(false);

		TextItem organizationalUnit = ItemFactory.newTextItem("organizationalunit", null);
		organizationalUnit.setHint("OU");
		organizationalUnit.setRequired(true);
		organizationalUnit.setWrapTitle(false);

		TextItem keytoolCommand = prepareKeytoolCommandItem();

		TextItem opensslCommand = prepareOpensslCommandItem();

		DynamicForm signatureForm = new DynamicForm();
		signatureForm.setValuesManager(vm);
		signatureForm.setWrapItemTitles(false);
		signatureForm.setIsGroup(true);
		signatureForm.setWidth(1);
		signatureForm.setAlign(Alignment.LEFT);
		signatureForm.setGroupTitle(I18N.message("signature"));

		TextItem exprx = ItemFactory.newTextItem("exprx", keystore != null ? keystore.getSignX() : "");
		exprx.setWidth(300);

		TextItem expry = ItemFactory.newTextItem("expry", keystore != null ? keystore.getSignY() : "");
		expry.setWidth(300);

		TextItem width = ItemFactory.newTextItem("width", keystore != null ? keystore.getSignWidth() : "");
		width.setWidth(300);

		ToggleItem visual = ItemFactory.newToggleItem("visual", keystore != null && keystore.isSignVisual());

		SpinnerItem opacity = ItemFactory.newSpinnerItem("opacity", keystore != null ? keystore.getSignOpacity() : 100,
				1, 100);

		RichTextItem text = ItemFactory.newRichTextItemForAutomation("text", "text",
				keystore != null ? keystore.getSignText() : "", null);

		signatureForm.setItems(visual, exprx, expry, width, opacity, text);

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(5);
		buttons.setMargin(3);

		IButton createNew = prepareCreateNewButton();

		IButton save = prepareSaveButton();

		IButton export = prepareExportButton();

		IButton delete = prepareDeleteButton();

		IButton importButton = prepareImportButton();

		IButton downloadCert = prepareDownloadCertButton();

		/*
		 * Two invisible fields to 'mask' the real credentials to the browser
		 * and prevent it to auto-fill the username and password we really use.
		 */
		TextItem fakeUsername = ItemFactory.newTextItem("prevent_autofill", "xyz");
		fakeUsername.setCellStyle("nodisplay");
		PasswordItem fakePassword = ItemFactory.newPasswordItem("password_fake", "password_fake", "xyz");
		fakePassword.setCellStyle("nodisplay");

		if (ksAlreadyGenerated) {
			password.setRequired(false);
			passwordAgain.setRequired(false);
			if (tenantId == Constants.TENANT_DEFAULTID)
				form.setItems(fakeUsername, fakePassword, details, validity, localCAalias, password, passwordAgain,
						keytoolCommand, opensslCommand);
			else
				form.setItems(fakeUsername, fakePassword, details, validity, fakeUsername, fakePassword, localCAalias,
						password, passwordAgain);
			layout.addMember(form);
			layout.addMember(signatureForm);

			buttons.setMembers(save, export, downloadCert, importButton, delete);
		} else {
			Label label = new Label(I18N.message("keystorenotgenerated"));
			label.setHeight(50);
			layout.addMember(label);

			if (tenantId == Constants.TENANT_DEFAULTID)
				form.setItems(fakeUsername, fakePassword, validity, fakeUsername, fakePassword, localCAalias, password,
						passwordAgain, organization, organizationalUnit, countryCode, keytoolCommand, opensslCommand);
			else
				form.setItems(fakeUsername, fakePassword, validity, fakeUsername, fakePassword, localCAalias, password,
						passwordAgain, organization, organizationalUnit, countryCode);

			layout.addMember(form);
			layout.addMember(signatureForm);

			buttons.setMembers(createNew, importButton);
		}
		layout.addMember(buttons);

		addMember(layout);
	}

	private TextItem prepareOpensslCommandItem() {
		TextItem opensslCommand = ItemFactory.newTextItem("opensslCommand", "OpenSSL",
				keystore != null ? keystore.getOpenSSLPath() : "");
		opensslCommand.setWidth(400);
		opensslCommand.setRequired(true);
		opensslCommand.setColSpan(2);
		return opensslCommand;
	}

	private TextItem prepareKeytoolCommandItem() {
		TextItem keytoolCommand = ItemFactory.newTextItem("keytoolCommand", "Keytool",
				keystore != null ? keystore.getKeytoolPath() : "");
		keytoolCommand.setWidth(400);
		keytoolCommand.setRequired(true);
		keytoolCommand.setColSpan(2);
		return keytoolCommand;
	}

	private SpinnerItem prepareValiditySpinner() {
		SpinnerItem validity = ItemFactory.newSpinnerItem("validity", keystore != null ? keystore.getValidity() : 2);
		validity.setHint(I18N.message("years"));
		validity.setRequired(true);
		validity.setMin(1);
		validity.setMax(10);
		validity.setStep(1);
		return validity;
	}

	private TextItem prepareLocalCAaliasItem() {
		TextItem localCAalias = ItemFactory.newSimpleTextItemPreventAutocomplete("localCAalias", "localcaalias",
				keystore != null ? keystore.getOrganizationAlias() : null);
		localCAalias.setRequired(true);
		localCAalias.setSelectOnFocus(true);
		localCAalias.setWrapTitle(false);
		return localCAalias;
	}

	private boolean prepareForm() {
		boolean ksAlreadyGenerated = keystore != null && keystore.getId() != 0L;
		vm.clearValues();
		vm.clearErrors(false);

		if (form != null)
			form.destroy();

		if (layout != null) {
			layout.destroy();
		}

		layout = new VLayout();
		layout.setWidth100();

		form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);
		return ksAlreadyGenerated;
	}

	private StaticTextItem prepareDetailsItem() {
		StaticTextItem details = ItemFactory.newStaticTextItem("details",
				(keystore != null && keystore.getOrganizationDN() != null)
						? (keystore.getOrganizationDN() + " " + I18N.message("validtill") + ": "
								+ I18N.formatDate(keystore.getOrganizationExpire()))
						: I18N.message("error").toLowerCase());
		details.setWrap(false);
		details.setColSpan(2);
		return details;
	}

	private IButton prepareDownloadCertButton() {
		IButton downloadCert = new IButton(I18N.message("downloadcert"));
		downloadCert.setAutoFit(true);
		downloadCert.addClickHandler(
				event -> Util.download(Util.contextPath() + "export-keystore?cert=true&tenantId=" + tenantId));
		return downloadCert;
	}

	private IButton prepareImportButton() {
		IButton button = new IButton(I18N.message("iimport"));
		button.setAutoFit(true);
		button.addClickHandler(event -> new KeystoreUploader(TenantKeystorePanel.this).show());
		return button;
	}

	private IButton prepareDeleteButton() {
		IButton delete = new IButton(I18N.message("ddelete"));
		delete.setAutoFit(true);
		delete.addClickHandler(event -> SC.ask(I18N.message("deletekeystorewarn"), answer -> {
			if (Boolean.TRUE.equals(answer))
				SignService.Instance.get().deleteKeystore(tenantId, new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(Void arg) {
						initGUI();
					}
				});
		}));
		return delete;
	}

	private IButton prepareExportButton() {
		IButton export = new IButton(I18N.message("export"));
		export.setAutoFit(true);
		export.addClickHandler(event -> Util.download(Util.contextPath() + "export-keystore?tenantId=" + tenantId));
		return export;
	}

	private IButton prepareSaveButton() {
		IButton save = new IButton(I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(event -> {
			if (!validate())
				return;

			LD.contactingServer();
			SignService.Instance.get().saveKeystore(keystore, new EmptyAsyncCallback<>());
		});
		return save;
	}

	private IButton prepareCreateNewButton() {
		IButton createNew = new IButton(I18N.message("generatenewkeystore"));
		createNew.setAutoFit(true);
		createNew.addClickHandler(event -> {
			if (!validate())
				return;
			LD.contactingServer();
			SignService.Instance.get().generateNewKeystore(keystore, new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(Void arg) {
					initGUI();
				}
			});
		});
		return createNew;
	}

	boolean validate() {
		if (Boolean.TRUE.equals(vm.validate())) {
			keystore.setTenantId(tenantId);
			keystore.setOrganizationAlias(vm.getValueAsString("localCAalias"));
			keystore.setValidity(Integer.parseInt(vm.getValueAsString("validity")));
			keystore.setSignX(vm.getValueAsString("exprx"));
			keystore.setSignY(vm.getValueAsString("expry"));
			keystore.setSignWidth(vm.getValueAsString("width"));
			keystore.setSignVisual(Boolean.valueOf(vm.getValueAsString("visual")));
			keystore.setSignOpacity(Integer.parseInt(vm.getValueAsString("opacity")));
			keystore.setSignText(vm.getValueAsString("text"));
			keystore.setPassword(vm.getValueAsString(PASSWORD));
			keystore.setKeytoolPath(vm.getValueAsString("keytoolCommand"));
			keystore.setOpenSSLPath(vm.getValueAsString("opensslCommand"));
			try {
				keystore.setOrganizationDN(
						"O=" + vm.getValueAsString("organization") + ",OU=" + vm.getValueAsString("organizationalunit")
								+ ",C=" + vm.getValueAsString("countrycode").toUpperCase());
			} catch (Exception t) {
				// Nothing to do
			}
			return true;
		} else {
			return false;
		}
	}

	public GUIKeystore getKeystore() {
		return keystore;
	}

	public long getTenantId() {
		return tenantId;
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