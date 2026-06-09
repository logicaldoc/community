package com.logicaldoc.gui.frontend.client.ai.model;

import java.io.Serializable;

/**
 * Describes a feature
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3
 */
public class GUIFeatureDescriptor implements Serializable {

    private static final long serialVersionUID = 1L;

    String name;

    String type = "java.lang.String";

    public GUIFeatureDescriptor() {
        // Empty constructor
    }
    
    public GUIFeatureDescriptor(String name) {
        super();
        this.name = name;
    }
    
    public GUIFeatureDescriptor(String name, String type) {
        super();
        this.name = name;
        this.type = type;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }    
}