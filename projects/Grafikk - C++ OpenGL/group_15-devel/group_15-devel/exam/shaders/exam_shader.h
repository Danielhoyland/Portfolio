const std::string vertexShaderSrc = R"(
        #version 430 core

        layout(location = 0) in vec2 i_position;
        layout(location = 1) in vec2 i_texCoord;         
        
        out vec2 fragTexCoord; // check if this is needed TODO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        out vec2 pos;
        out vec4 vs_fragPosition;
        out vec4 vs_normal;


        uniform mat4 u_projectionView;
        uniform mat4 u_model;
    
        void main()
        {
            gl_Position = u_projectionView * u_model * vec4(i_position, 0.0, 1.0);
            fragTexCoord = i_texCoord;
            pos = i_position;
            vs_fragPosition = u_model * vec4(i_position, 0.0, 1.0);
            vs_normal = normalize(u_model * vec4(0.0, 0.0, 1.0, 1.0));
        }
)";

const std::string fragmentShaderSrc = R"(
        #version 430 core

        layout(binding = 0) uniform sampler2D u_texture;
        const float M_PI = 3.14159265359;

        in vec3 fragColor;
        in vec2 fragTexCoord;
        in vec3 fragMarkerColor;
        in vec2 pos;
        in vec4 vs_fragPosition;
        in vec4 vs_normal;

        uniform bool u_toggle;
        uniform bool u_mode;
        uniform int u_gridsize;
        // lighting
        uniform float u_ambientStrength;
        uniform vec3 u_lightSourcePosition;
        uniform float u_diffuseStrength;
        uniform float u_specularStrength;
        uniform vec3 u_cameraPosition;
        
        out vec4 color;

        void main()
        {   
            int s = u_gridsize;
            if ((sin ((pos.x*s*M_PI)) > 0) && (sin ((pos.y*s*M_PI)) > 0)) {
                color = vec4(0.4, 0.4, 0.4, 1.0);
            } else if ((sin ((pos.x*s*M_PI)) < 0) && (sin ((pos.y*s*M_PI))) > 0){
                color = vec4(0.7, 0.7, 0.7, 1.0);
            } else if ((sin ((pos.x*s*M_PI)) > 0) && (sin ((pos.y*s*M_PI))) < 0){
                color = vec4(0.7, 0.7, 0.7, 1.0);
            } else if ((sin ((pos.x*s*M_PI)) < 0) && (sin ((pos.y*s*M_PI))) < 0){
                color = vec4(0.4, 0.4, 0.4, 1.0);
            }
            if(u_mode){
                color = mix(color, texture(u_texture, fragTexCoord), 0.4);
            }
            
            // diffuse lighting
            vec3 lightDirection = normalize(vec3(u_lightSourcePosition - vs_fragPosition.xyz)); // the light "bounce"
            float diffuseStrength = max(dot(lightDirection, vs_normal.xyz), 0.0) * 0.2;

            // Specular illumination
            vec3 reflectedLight = normalize(reflect(-lightDirection, vs_normal.xyz));
            vec3 observerDirection = normalize(u_cameraPosition - vs_fragPosition.xyz);
            float specFactor = pow(max(dot(observerDirection, reflectedLight), 0.0), 12);
            float specular = specFactor * u_specularStrength;

            color = vec4(color.rgb, 1.0) * (u_ambientStrength + diffuseStrength + specular);
            color = vec4(color.rgb, 1.0);
        }
)";

/*--------------- Defining Cube Shaders ---------------------*/

const std::string cubeVertexShaderSrc = R"(
        #version 430 core

        uniform mat4 u_projectionView;
        uniform mat4 u_model;

        layout(location = 0) in vec3 i_position;
        layout(location = 1) in vec3 i_normal;
        
        out vec3 i_pos;
        out vec3 vs_normal;
        out vec3 vs_fragPosition;
    
        void main()
        {   
            gl_Position = u_projectionView * u_model * vec4(i_position, 1.0);
            i_pos = i_position;
            vs_normal = i_normal;
            vs_fragPosition = vec3(u_model * vec4(i_position, 1.0));
        }
    )";

const std::string cubeFragmentShaderSrc = R"(
        #version 430 core
        const float M_PI = 3.14159265359;

        uniform vec3 u_color;
        uniform bool u_mode;
        uniform int u_type;
        // lighting
        uniform float u_ambientStrength;
        uniform vec3 u_lightSourcePosition;
        uniform float u_diffuseStrength;
        uniform float u_specularStrength;
        uniform vec3 u_cameraPosition;        

        layout(binding = 1) uniform samplerCube boxTex;
        layout(binding = 2) uniform samplerCube wallTex;

        in vec3 i_pos;
        in vec3 vs_fragPosition;
        in vec3 vs_normal;

        out vec4 color;

        void main()
        {      
            float opacity = 1.0;
            color = vec4(u_color, 1.0);

            // Diffuse illumination
            vec3 norm = normalize(vs_normal);
            vec3 lightDirection = normalize(u_lightSourcePosition - vs_fragPosition.xyz);
            float diffuseStrength = max(dot(vs_normal, lightDirection), 0.0f) * u_diffuseStrength;
            
            // Specular illumination
            vec3 reflectedLight = normalize(reflect(-lightDirection, vs_normal.xyz));
            vec3 observerDirection = normalize(u_cameraPosition - vs_fragPosition.xyz);
            float specFactor = pow(max(dot(observerDirection, reflectedLight), 0.0), 5);
            float specular = specFactor * u_specularStrength;

            // setting opacity for each object
            switch(u_type){
            case 0: opacity = 0.4; break;
            case 1: opacity = 1.0; break;
            case 2: opacity = 1.0; break;
            case 3: opacity = 1.0; break; 
            case 4: opacity = 1.0; break;  
            }

            // if texture mode, set textures
            if(u_mode){
                if(u_type == 1){
                    color = mix(vec4(u_color, 0.0), texture(wallTex, i_pos), 0.5); 
                }
                else if (u_type == 3){
                    color = mix(vec4(u_color, 0.0), texture(boxTex, i_pos), 0.5);
                } 
            }
            // if cube is the sun, ignore lighting other than ambient
            if(u_type != 4){
                color = vec4(color.rgb, 1.0) * (u_ambientStrength + (diffuseStrength) + specular);
            }else{
                color = vec4(color.rgb, 1.0) * 2.0f;
            }
            color = vec4(color.rgb, opacity);
        }
    )";

