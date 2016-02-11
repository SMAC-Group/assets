library(gmwm)
help(package = "gmwm")

path = "/Users/robertomolinari/Box Sync/Eurocow 2016/"

# 1
model = GM(beta = 0.5, sigma2_gm = 5*10^(-4)) + WN(sigma2=0.01)
sim = gen.gts(model, 10^6, freq = 100)

wv = wvar(sim)
plot(wv)

# 2
example.1 = read.imu(paste(path,"fsas_imar_short_32min.imu", sep=""), type = "IMAR")
plot(wvar(example.1))

gyro.y = example.1$data[,2]

wv.cl = wvar(gyro.y)
wv.rob = wvar(gyro.y, robust = T)

compare.wvar(wv.cl,wv.rob)
compare.wvar(wv.cl,wv.rob, split=F)

example.2 = read.table(paste(path,"mtig_xsens_30min.txt", sep=""), row.names = 1)
is.imu(example.2)
example.2 = imu(example.2, gyroscope = 1:3, accelerometer = 4:6 , freq = 100)

# 3
gyro.x = imu(example.2$data, gyroscope = 1, freq=100)
plot(wvar(gyro.x))

model.1 = gmwm.imu(GM()+WN(),gyro.x)
plot(model.1)
model.2 = gmwm.imu(2*GM()+WN(),gyro.x)
plot(model.2)
model.3 = gmwm.imu(3*GM()+WN(),gyro.x)
plot(model.3)
model.4 = gmwm.imu(2*GM()+WN()+RW(),gyro.x)
plot(model.4)

compare.models(model.1,model.2,model.3,model.4)

model.2$estimate
summary(model.2, inference=T)

# 4
select.mod.1 = auto.imu(gyro.x, 3*GM()+WN()+RW())
select.mod.1
plot(select.mod.1)

select.mod.2 = auto.imu(gyro.y, 3*GM()+WN()+RW())
select.mod.2
plot(select.mod.2)