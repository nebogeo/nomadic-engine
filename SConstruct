env = Environment(CCFLAGS='-O3 -std=gnu++0x -ggdb -DUSE_MATH=1 -Wno-write-strings')
target = ARGUMENTS.get('TARGET','LINUX')

if target=='LINUX':
   env.Append(LIBS = ['glut', 'GL', 'png'])
   env.Append(CCFLAGS=' -DFLX_LINUX')
   env.Append(CPPPATH = '.')

if target=='RPI':
   # raspberry pi
   env.Append(LIBS = ['glutes', 'GLESv1_CM', 'EGL', 'bcm_host', 'X11', 'png'])
   env.Append(CCFLAGS=' -DFLX_RPI -fpermissive')
   env.Append(CPPPATH = '/opt/vc/include:.')
   env.Append(LIBPATH = '/opt/vc/lib')

env.Program( target = 'nomadic',
             source = ['main.cpp',
                       'core/fixed.cpp',
                       'core/list.cpp',
                       'core/geometry.cpp',
                       'engine/engine.cpp',
                       'engine/primitive.cpp',
                       'engine/text_primitive.cpp',
                       'engine/scenegraph.cpp',
                       'engine/scenenode.cpp',
                       'engine/texture.cpp',
                       'engine/nomadic.cpp',
                       'scheme/scheme.cpp',
                       'jellyfish/jellyfish_primitive.cpp',
                       'jellyfish/jellyfish.cpp'
                       ] )
