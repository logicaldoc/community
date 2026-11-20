package com.logicaldoc.gui.frontend.client.metadata.template;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.Upload;
import com.logicaldoc.gui.frontend.client.services.TemplateService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SubmitItem;

/**
 * This popup window is used to upload a new template file to the server.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3.2
 */
public class TemplateUploader extends Window {

    private Upload uploader;

    private TemplatesPanel panel;

    public TemplateUploader(TemplatesPanel panel) {
        this.panel = panel;
        setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
        setTitle(I18N.message("uploadtemplate"));
        setWidth(400);
        setHeight(160);
        setCanDragResize(true);
        setIsModal(true);
        setShowModalMask(true);
        centerInPage();

        DynamicForm form = new DynamicForm();
        SubmitItem submitButton = new SubmitItem();
        submitButton.setTitle(I18N.message("submit"));
        submitButton.setDisabled(true);
        submitButton.setAlign(Alignment.RIGHT);
        submitButton.addClickHandler(event -> onSubmit());

        form.setItems(submitButton);

        uploader = new Upload(submitButton);
        addItem(uploader);
        addItem(form);
    }

    public void onSubmit() {
        if (uploader.getUploadedFile() == null) {
            SC.warn(I18N.message("filerequired"));
            return;
        }

        LD.contactingServer();
        TemplateService.Instance.get().importTemplate(new DefaultAsyncCallback<>() {
            @Override
            public void handleSuccess(Void set) {
                panel.refresh();
                destroy();
            }
        });
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