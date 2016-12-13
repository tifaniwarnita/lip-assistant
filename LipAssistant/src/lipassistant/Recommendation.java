/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package lipassistant;

/**
 *
 * @author ASUS X202E
 */
public class Recommendation {
    private String name;
    private String explanation;

    public Recommendation(String name, String explanation) {
        this.name = name;
        this.explanation = explanation;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getExplanation() {
        return explanation;
    }

    public void setExplanation(String explanation) {
        this.explanation = explanation;
    }
    
}
