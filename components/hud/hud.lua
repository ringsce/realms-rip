-- Function to initialize HUD
function initHUD()
    print("Initializing HUD...")
    -- Set up HUD elements, such as labels, buttons, etc.
    hudElements = {
        lifeLabel = "Life: 100%",
        statusLabel = "Status: Normal",
        powderLabel = "Powder: 30",
        staminaLabel = "Stamina: 100%",
        xpLabel = "XP: 0"
    }
end

-- Function to update HUD elements
function updateHUD(life, status, powder, stamina, xp)
    -- Update HUD elements with new information
    hudElements.lifeLabel = "Life: " .. life .. "%"
    hudElements.statusLabel = "Status: " .. status
    hudElements.powderLabel = "Powder: " .. powder
    hudElements.staminaLabel = "Stamina: " .. stamina .. "%"
    hudElements.xpLabel = "XP: " .. xp
end

-- Function to render HUD
function renderHUD()
    -- Render HUD elements on the screen
    print(hudElements.lifeLabel)
    print(hudElements.statusLabel)
    print(hudElements.powderLabel)
    print(hudElements.staminaLabel)
    print(hudElements.xpLabel)
end

-- Main function
function main()
    -- Initialize HUD
    initHUD()

    -- Simulate updating HUD elements
    updateHUD(75, "Injured", 20, 80, 500)

    -- Render HUD
    renderHUD()
end

-- Call the main function to start the HUD
main()
