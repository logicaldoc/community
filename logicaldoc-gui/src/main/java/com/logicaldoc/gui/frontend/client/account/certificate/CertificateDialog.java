package com.logicaldoc.gui.frontend.client.account.certificate;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.controllers.UserController;
import com.logicaldoc.gui.common.client.controllers.UserObserver;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.SignService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This is the form used to display and generate the user's certificate
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.2
 */
public class CertificateDialog extends Window implements UserObserver {

	private VLayout layout = null;

	public CertificateDialog() {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("certificate"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		UserController.get().addObserver(this);

		refresh();
	}

	public void refresh() {
		boolean crtAlreadyGenerated = Session.get().getUser().getCertDN() != null;

		if (layout != null) {
			layout.destroy();
		}

		layout = new VLayout();
		layout.setWidth100();
		layout.setHeight100();

		DynamicForm form = new DynamicForm();
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(2);
		form.setMinWidth(450);

		StaticTextItem details = ItemFactory.newStaticTextItem("details", Session.get().getUser().getCertDN() + " "
				+ I18N.message("validtill") + ": " + I18N.formatDate(Session.get().getUser().getCertExpire()));
		details.setColSpan(2);
		details.setWrap(true);

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(5);
		buttons.setMargin(3);

		IButton createNew = new IButton(I18N.message("generatecert"));
		createNew.setAutoFit(true);
		createNew.addClickHandler(event -> {
			LD.contactingServer();
			SignService.Instance.get().generateNewCertificate(new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(Void arg) {
					SecurityService.Instance.get().getUser(Session.get().getUser().getId(), new DefaultAsyncCallback<>() {
						@Override
						public void handleSuccess(GUIUser user) {
							Session.get().setUser(user);
							refresh();
						}
					});
				}
			});
		});

		IButton upload = new IButton(I18N.message("uploadyourowncert"));
		upload.setAutoFit(true);
		upload.addClickHandler(event -> new OwnCertificateDialog().show());

		IButton delete = new IButton(I18N.message("deletecert"));
		delete.setAutoFit(true);
		delete.addClickHandler(event -> SC.ask(I18N.message("deletecertwarn"), (Boolean value) -> {
			LD.contactingServer();
			SignService.Instance.get().deleteCertificate(new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(Void arg) {
					Session.get().getUser().setCertDN(null);
					Session.get().getUser().setCertExpire(null);
					refresh();
				}
			});
		}));

		IButton download = new IButton(I18N.message("download"));
		download.setAutoFit(true);
		download.addClickHandler(event -> Util.download(Util.contextPath() + "export-keystore?cert="
				+ Session.get().getUser().getUsername() + "&tenantId=" + Session.get().getTenantId()));

		if (crtAlreadyGenerated) {
			form.setItems(details);
			layout.addMember(form);
			buttons.setMembers(delete, download);
		} else {
			Label label = new Label(I18N.message("youdonthavecert"));
			label.setHeight(50);
			label.setWrap(false);
			layout.addMember(label);
			buttons.setMembers(createNew, upload);
		}
		layout.addMember(buttons);

		addItem(layout);
	}

	@Override
	public void onUserChanged(GUIUser user) {
		refresh();
	}

	@Override
	public void onUserLogin(String username) {
		// Nothing to do
	}

	@Override
	public void onUserLogout(String username) {
		// Nothing to do
	}

	@Override
	protected void onUnload() {
		UserController.get().removeObserver(this);
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