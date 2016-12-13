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
public class Preference {
    private String shape;
    private String texture;
    private String price;
    private String skintone;
    
    public Preference() {
        
    }

    public Preference(String shape, String texture, String price, String skintone) {
        this.shape = shape;
        this.texture = texture;
        this.price = price;
        this.skintone = skintone;
    }

    public String getShape() {
        return shape;
    }

    public void setShape(String shape) {
        this.shape = shape;
    }

    public String getTexture() {
        return texture;
    }

    public void setTexture(String texture) {
        this.texture = texture;
    }

    public String getPrice() {
        return price;
    }

    public void setPrice(String price) {
        this.price = price;
    }

    public String getSkintone() {
        return skintone;
    }

    public void setSkintone(String skintone) {
        this.skintone = skintone;
    }
    
    
}
